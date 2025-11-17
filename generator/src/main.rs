use clap::Parser;
use colored::Colorize;
use generator::cli::Cli;
use rand::{Rng, SeedableRng, rngs::StdRng};
use std::{
    fmt::Display,
    io::{self, BufWriter, Write},
    net::TcpStream,
};

type Account = String;
type Amount = u32;

#[derive(Debug)]
struct Transaction {
    seq: u32,
    action: Action,
}

#[derive(Debug)]
enum Action {
    OpenAccount(Account),
    Deposit(Account, Amount),
    Transfer(Account, Account, Amount),
}

impl Display for Transaction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Transaction (seq: {}) => {}", self.seq, self.action)
    }
}

impl Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OpenAccount(account) => write!(f, "OpenAccount {}", account),
            Self::Deposit(to, amount) => write!(f, "Deposit {} to {}", amount, to),
            Self::Transfer(from, to, amount) => {
                write!(f, "Transfer {} from {} to {}", amount, from, to)
            }
        }
    }
}

fn gen_account(rng: &mut StdRng) -> String {
    let x = rng.random_range(99_999_999_999..1_000_000_000_000u64);
    format!("{:012}", x)
}

fn pick_account<'r>(rng: &mut StdRng, accounts: &'r Vec<String>) -> &'r String {
    let i = rng.random_range(0..accounts.len());
    &accounts.get(i).unwrap() // Absolutely fail!
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
    let mut writer: BufWriter<Box<dyn Write>> = if cli.output_stdout {
        BufWriter::new(Box::new(io::stdout()))
    } else {
        match TcpStream::connect(cli.server) {
            Ok(stream) => BufWriter::new(Box::new(stream)),
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

    // Open them
    //
    accounts.iter().for_each(|account| {
        let open_tx = Transaction {
            seq: seqg.next().expect("seq should never fail"),
            action: Action::OpenAccount(account.clone()),
        };
        if cli.verbose {
            eprintln!("Sending [{}]", open_tx);
        }
        writeln!(writer, "{}", open_tx).expect("Write failed");

        let deposit_tx = Transaction {
            seq: seqg.next().expect("seq should never fail"),
            action: Action::Deposit(account.clone(), gen_amount(&mut rng, &ACCOUNT_START_MAX)),
        };
        if cli.verbose {
            eprintln!("Sending [{}]", deposit_tx);
        }
        writeln!(writer, "{}", deposit_tx).expect("Write failed");
    });

    eprintln!("Accounts sent!");

    // Transfers
    //
    (0..cli.num_transfers).for_each(|_| {
        let from = pick_account(&mut rng, &accounts);
        let to = pick_account(&mut rng, &accounts);
        let amount = gen_amount(&mut rng, &TRANSFER_MAX);
        let transfer_tx = Transaction {
            seq: seqg.next().expect("seq should never fail"),
            action: Action::Transfer(from.clone(), to.clone(), amount),
        };

        if transfer_tx.seq % 10_000_000 == 0 {
            eprintln!("  Sent {} total messages...", transfer_tx.seq);
        }

        if cli.verbose {
            eprintln!("Sending [{}]", transfer_tx);
        }
        writeln!(writer, "{}", transfer_tx).expect("Write failed");
    });

    eprintln!("Done! ({} total messages)", seqg.next().unwrap());
}
