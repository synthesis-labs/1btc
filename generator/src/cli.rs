use clap::Parser;

#[derive(Parser)]
#[command(name = "generator")]
#[command(about = "Generate transaction data!")]
pub struct Cli {
    #[arg(long, help = "Server address and port", default_value_t = String::from("127.0.0.1:7077"))]
    pub server: String,
    #[arg(long, help = "Number of accounts", default_value_t = 200_000)]
    pub num_accounts: u64,
    #[arg(long, help = "Number of transfers", default_value_t = 1_000_000_000)]
    pub num_transfers: u64,
    #[arg(long, help = "Verbose output", default_value_t = false)]
    pub verbose: bool,
    #[arg(
        long,
        help = "Output to stdout (instead of socket)",
        default_value_t = false
    )]
    pub output_stdout: bool,
    #[arg(
        long,
        help = "Output compact text format (less bytes on the wire)",
        default_value_t = false
    )]
    pub compact: bool,

    #[arg(
        long,
        help = "Number of concurrent connections to make",
        default_value_t = 1
    )]
    pub connections: usize,
    #[arg(
        long,
        help = "Number of sequential transactions to send at a time",
        default_value_t = 1_000_000_000
    )]
    pub batch: usize,
}
