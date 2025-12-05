use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::net::TcpListener;

mod parser_nom;
mod parser_nom_compact;
mod parser_raw_compact;
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
                // Using nom parser:
                //
                // match parser_nom::parse(&line) {
                //     Ok(transaction) => {} // println!("{:?}", transaction),
                //     Err(e) => println!("Error parsing line: {}", e),
                // }
                // Using nom parser (compact):
                //
                // let x = parser_nom_compact::parse(&line);
                let x = parser_raw_compact::parse(&line);
            }

            println!("Connection closed: {}", addr);
        });
    }
}
