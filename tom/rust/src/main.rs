use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::net::TcpListener;

mod parser;
mod types;
use types::{Action, Transaction};

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:7077").await?;
    println!("Listening on port 7077");

    loop {
        let (socket, addr) = listener.accept().await?;
        println!("New connection from: {}", addr);

        tokio::spawn(async move {
            let reader = BufReader::new(socket);
            let mut lines = reader.lines();

            while let Ok(Some(line)) = lines.next_line().await {
                // match parser::parse_line(&line) {
                //     Ok(transaction) => {} // println!("{:?}", transaction),
                //     Err(e) => println!("Error parsing line: {}", e),
                // }
            }

            println!("Connection closed: {}", addr);
        });
    }
}
