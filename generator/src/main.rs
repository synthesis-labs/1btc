use clap::Parser;
use generator::cli::Cli;
use rand::{Rng, SeedableRng, rngs::StdRng};
use std::{
    fmt::Write as FmtWrite,
    io::{self, BufWriter, Write},
    net::TcpStream,
};

fn gen_account(rng: &mut StdRng) -> String {
    let x = rng.random_range(99_999_999_999..1_000_000_000_000u64);
    format!("{:012}", x)
}

fn pick_account_idx(rng: &mut StdRng, num_accounts: usize) -> usize {
    rng.random_range(0..num_accounts)
}

fn gen_amount(rng: &mut StdRng, max: &u32) -> u32 {
    // Attempt to weight the values exponentially downwards
    //
    let u: f64 = rng.random(); // 0.0 to 1.0 uniformly
    let skewed = u.powi(3); // higher means more weighting
    (skewed * *max as f64) as u32 + 1
}

const ACCOUNT_START_MAX: u32 = 99_999_999;
const TRANSFER_MAX: u32 = 999_999;

fn main() {
    let cli = Cli::parse();

    eprintln!("Generating transactions and firing them to {}", cli.server);

    let mut rng = StdRng::seed_from_u64(12345);
    let mut seqg = (0..).map(|s| s as u32);

    // Open a bufwriter to either the socket or stdout
    //
    const BUFFER_SIZE: usize = 4 * 1024 * 1024; // 4MB buffer
    let mut writer: BufWriter<Box<dyn Write>> = if cli.output_stdout {
        BufWriter::with_capacity(BUFFER_SIZE, Box::new(io::stdout()))
    } else {
        match TcpStream::connect(cli.server) {
            Ok(stream) => BufWriter::with_capacity(BUFFER_SIZE, Box::new(stream)),
            Err(e) => {
                eprintln!("Connection failed: {}", e);
                std::process::exit(1);
            }
        }
    };

    // Generate a bunch of accounts
    //
    let accounts: Vec<String> = (0..)
        .map(|_| gen_account(&mut rng))
        .take(cli.num_accounts as usize)
        .collect();

    // Reusable buffer for formatting messages
    let mut msg_buf = String::with_capacity(256);

    // Open them
    //
    for account in &accounts {
        let seq = seqg.next().expect("seq should never fail");
        msg_buf.clear();
        write!(&mut msg_buf, "Transaction (seq: {}) => OpenAccount {}\n", seq, account).unwrap();
        if cli.verbose {
            eprint!("Sending [{}]", msg_buf.trim_end());
        }
        writer.write_all(msg_buf.as_bytes()).expect("Write failed");

        let seq = seqg.next().expect("seq should never fail");
        let amount = gen_amount(&mut rng, &ACCOUNT_START_MAX);
        msg_buf.clear();
        write!(&mut msg_buf, "Transaction (seq: {}) => Deposit {} to {}\n", seq, amount, account).unwrap();
        if cli.verbose {
            eprint!("Sending [{}]", msg_buf.trim_end());
        }
        writer.write_all(msg_buf.as_bytes()).expect("Write failed");
    }

    eprintln!("Accounts sent!");

    // Transfers
    //
    let num_accounts = accounts.len();
    for _ in 0..cli.num_transfers {
        let from_idx = pick_account_idx(&mut rng, num_accounts);
        let to_idx = pick_account_idx(&mut rng, num_accounts);
        let amount = gen_amount(&mut rng, &TRANSFER_MAX);
        let seq = seqg.next().expect("seq should never fail");

        if seq % 10_000_000 == 0 {
            eprintln!("  Sent {} total messages...", seq);
        }

        msg_buf.clear();
        write!(&mut msg_buf, "Transaction (seq: {}) => Transfer {} from {} to {}\n",
               seq, amount, &accounts[from_idx], &accounts[to_idx]).unwrap();

        if cli.verbose {
            eprint!("Sending [{}]", msg_buf.trim_end());
        }
        writer.write_all(msg_buf.as_bytes()).expect("Write failed");
    }

    eprintln!("Done! ({} total messages)", seqg.next().unwrap());
}
