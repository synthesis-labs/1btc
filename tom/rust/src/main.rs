use futures::StreamExt;
use tokio::net::TcpListener;
use tokio_util::codec::{FramedRead, LinesCodec};

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
            let mut framed = FramedRead::new(socket, LinesCodec::new());

            while let Some(result) = framed.next().await {
                // match result {
                //     Ok(line) => {
                //         match parser::parse_line(&line) {
                //             Ok(transaction) => {} // println!("{:?}", transaction),
                //             Err(e) => println!("Error parsing line: {}", e),
                //         }
                //     }
                //     Err(e) => {
                //         println!("Error reading line: {}", e);
                //         break;
                //     }
                // }
            }

            println!("Connection closed: {}", addr);
        });
    }
}
