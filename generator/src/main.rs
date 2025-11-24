use clap::Parser;
use generator::cli::Cli;
use generator::protocol;
use itoa::Buffer;
use rand::{Rng, SeedableRng, rngs::SmallRng};
use std::{
    io::{self, BufWriter, Write},
    net::TcpStream,
};

fn gen_account(rng: &mut SmallRng) -> u64 {
    // low to high (both inclusive) - fixed 12 digits
    //
    rng.random_range(100_000_000_000..=999_999_999_999u64)
}

fn pick_account_idx(rng: &mut SmallRng, num_accounts: u64) -> u64 {
    rng.random_range(0..num_accounts)
}

fn gen_amount(rng: &mut SmallRng, max: &u32) -> u32 {
    // Attempt to weight the values exponentially downwards
    //
    let u: f64 = rng.random(); // 0.0 to 1.0 uniformly
    let skewed = u.powi(3); // higher means more weighting
    (skewed * *max as f64) as u32 + 1
}

const ACCOUNT_START_MAX: u32 = 99_999_999;
const TRANSFER_MAX: u32 = 999_999;

fn derive_transaction_from_seq(
    seq: u64,
    cli: &Cli,
    accounts: &[u64],
    msg_buf: &mut String,
    itoa_buf: &mut Buffer,
) -> () {
    // Sequence number is the seed
    //
    // let mut rng = StdRng::seed_from_u64(seq);
    let mut rng = SmallRng::seed_from_u64(seq);

    // First 0..num_accounts transactions are OpenAccount
    //
    if seq < cli.num_accounts {
        protocol::write_open_account(
            cli.compact,
            msg_buf,
            itoa_buf,
            &seq,
            &accounts[seq as usize],
        );
    }
    // Next num_accounts..(num_accounts*2) transactions are Deposit
    //
    else if seq < cli.num_accounts * 2 {
        let amount = gen_amount(&mut rng, &ACCOUNT_START_MAX);
        protocol::write_deposit(
            cli.compact,
            msg_buf,
            itoa_buf,
            &seq,
            &accounts[(seq - cli.num_accounts) as usize],
            &amount,
        );
    }
    // Rest are all transfers
    //
    else {
        let from_idx = pick_account_idx(&mut rng, cli.num_accounts);
        let to_idx = pick_account_idx(&mut rng, cli.num_accounts);
        let amount = gen_amount(&mut rng, &TRANSFER_MAX);

        protocol::write_transfer(
            cli.compact,
            msg_buf,
            itoa_buf,
            &seq,
            &accounts[from_idx as usize],
            &accounts[to_idx as usize],
            &amount,
        );
    }
}

fn main() {
    let cli = Cli::parse();

    eprintln!("Generating transactions and firing them to {}", cli.server);

    let mut accounts_rng = SmallRng::seed_from_u64(12345);

    // Open a bufwriter to either the socket or stdout
    //
    const BUFFER_SIZE: usize = 16 * 1024 * 1024; // 16MB
    let mut writer: BufWriter<Box<dyn Write>> = if cli.output_stdout {
        BufWriter::with_capacity(BUFFER_SIZE, Box::new(io::stdout()))
    } else {
        match TcpStream::connect(cli.server.clone()) {
            Ok(stream) => BufWriter::with_capacity(BUFFER_SIZE, Box::new(stream)),
            Err(e) => {
                eprintln!("Connection failed: {}", e);
                std::process::exit(1);
            }
        }
    };

    // Generate a bunch of accounts
    //
    let accounts: Vec<u64> = (0..)
        .map(|_| gen_account(&mut accounts_rng))
        .take(cli.num_accounts as usize)
        .collect();

    // Reusable buffers for formatting messages
    let mut msg_buf = String::with_capacity(256);
    let mut itoa_buf = itoa::Buffer::new();

    // We only care about seq numbers now, everything is derivable
    //
    let total_txs = cli.num_accounts * 2 + cli.num_transfers;

    for seq in 0..total_txs {
        derive_transaction_from_seq(seq, &cli, &accounts, &mut msg_buf, &mut itoa_buf);

        if cli.verbose {
            eprint!("Sending [{}]", msg_buf.trim_end());
        }
        writer.write_all(msg_buf.as_bytes()).expect("Write failed");

        if seq > 0 && seq % 10_000_000 == 0 {
            eprintln!("  Sent {} total messages...", seq);
        }
    }

    eprintln!("Done! ({} total messages)", total_txs);
}
