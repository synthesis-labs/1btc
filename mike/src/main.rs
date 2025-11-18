use tokio_util::codec::Decoder;
use bytes::{BytesMut, Buf};
use std::{io, thread::ThreadId};
use tokio::net::{TcpStream, TcpListener};
use tokio_util::codec::Framed;
use tokio_stream::StreamExt;
use std::collections::HashMap;
// use memchr;

//stream has a collection of messages: Transaction (seq: 253067) => Transfer 48337 from 199216764739 to 343382529531

type Account = u64; //String;
type Amount = u32;

#[derive(Debug)]
pub struct Transaction {
    seq: u32,
    action: Action,
}

#[derive(Debug)]
pub enum Action {
    OpenAccount(Account),
    Deposit(Account, Amount),
    Transfer(Account, Account, Amount),
}

#[derive(Debug)]
enum ParseError {
    Utf8(std::str::Utf8Error),
    Int(std::num::ParseIntError),
    Format(&'static str),
}

impl From<std::str::Utf8Error> for ParseError {
    fn from(e: std::str::Utf8Error) -> Self { ParseError::Utf8(e) }
}

impl From<std::num::ParseIntError> for ParseError {
    fn from(e: std::num::ParseIntError) -> Self { ParseError::Int(e) }
}

#[derive(Default)]
pub struct LfTerminatedCodec;

impl Decoder for LfTerminatedCodec {
    // type Item = BytesMut;      // or your own struct if you parse inside here
    type Item = Transaction;
    type Error = io::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if let Some(pos) = memchr::memchr(b'\n', src) {
                        let line = &src[..pos];

            // Parse the line
            let tx = parse_transaction2(line)
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format! ("{:?}", e)))?;

            // Drop the line + '\n' from the buffer by advancing the cursor over it
            src.advance(pos + 1);

            return Ok(Some(tx));
        }

        Ok(None)

        }


        // if let Some(pos) = src.iter().position(|b| *b == b'\n') {
        //     // split_to includes everything up to pos
        //     let mut frame = src.split_to(pos + 1);

        //     // // Drop the trailing '\n'
        //     // if frame.last() == Some(&b'\n') {
        //     //     frame.truncate(frame.len() - 1);
        //     // }

        //     // println!("Decoded frame: {:?}", frame);

        //     let transaction = parse_transaction2(&frame)
        //         .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("{:?}", e)))?;

        //     // println!("Parsed transaction: {:?}", transaction);

        //     return Ok(Some(transaction));
        // }

        // // No full frame yet
        // Ok(None)
    }




fn parse_transaction2(line: &[u8]) -> Result<Transaction, ParseError> {
    let s =unsafe {std::str::from_utf8_unchecked(&line[18..])};
    let mut parts = s.split_ascii_whitespace();
    
    let first  = parts.next().ok_or(ParseError::Format("Failed to find sequence"))?;
    let seq = &first[0..first.len()-1].parse()?;
    let _ = parts.next(); // skip '=>'
    let second = parts.next();
    let third = parts.next().ok_or(ParseError::Format("Failed to find third element"));
    let _ = parts.next();
    let fifth = parts.next().ok_or(ParseError::Format("Failed to find third element"));
    let _ = parts.next();
    let seventh = parts.next().ok_or(ParseError::Format("Failed to find third element"));


    // println!("Second: {:?}", third);

    match(second) {
        Some("OpenAccount") => Ok(Transaction{seq: *seq, action: Action::OpenAccount(third?.parse()?) }),
        Some("Deposit") => Ok(Transaction{seq: *seq, action: Action::Deposit(fifth?.parse()?, third?.parse()?) }),
        Some("Transfer") => Ok(Transaction{seq: *seq, action: Action::Transfer(fifth?.parse()?, seventh?.parse()?, third?.parse()?) }),
        Some(x) => Err(ParseError::Format("unknown action {}")),
        None => Err(ParseError::Format("missing action")),
    }
}

fn parse_transaction(line: &[u8]) -> Result<Transaction, ParseError> {
    
    let s = std::str::from_utf8(line)?;

    const PREFIX: &str = "Transaction (seq: ";
    let s = s
        .strip_prefix(PREFIX)
        .ok_or(ParseError::Format("missing prefix"))?;

    // seq is before ')'
    let (seq_str, rest) = s
        .split_once(')')
        .ok_or(ParseError::Format("missing closing ')'"))?;
    let seq: u32 = seq_str.trim().parse()?;

    // After ") => "
    let rest = rest
        .strip_prefix(" => ")
        .ok_or(ParseError::Format("missing ' => '"))?;

    // --- OpenAccount ---
    if let Some(account_str) = rest.strip_prefix("OpenAccount ") {
        let account: Account = account_str.trim().parse()?;
        return Ok(Transaction {
            seq,
            action: Action::OpenAccount(account),
        });
    }

    // --- Deposit <amount> to <account> ---
    if let Some(rest) = rest.strip_prefix("Deposit ") {
        let (amount_str, account_str) = rest
            .split_once(" to ")
            .ok_or(ParseError::Format("bad Deposit format"))?;

        let amount: Amount = amount_str.trim().parse()?;
        let account: Account = account_str.trim().parse()?;

        return Ok(Transaction {
            seq,
            action: Action::Deposit(account, amount),
        });
    }

    // --- Transfer <amount> from <src> to <dst> ---
    if let Some(rest) = rest.strip_prefix("Transfer ") {
        // amount before " from "
        let (amount_str, rest) = rest
            .split_once(" from ")
            .ok_or(ParseError::Format("missing ' from ' in Transfer"))?;
        let amount: Amount = amount_str.trim().parse()?;

        // now "<src> to <dst>"
        let (from_str, to_str) = rest
            .split_once(" to ")
            .ok_or(ParseError::Format("missing ' to ' in Transfer"))?;

        let from = from_str.trim().parse()?;
        let to = to_str.trim().parse()?;

        return Ok(Transaction {
            seq,
            action: Action::Transfer(from, to, amount),
        });
    }

    Err(ParseError::Format("unknown action"))
}


async fn handle_conn(stream: TcpStream) -> io::Result<()> {
    println!("New connection from: {}", stream.peer_addr()?);
    let mut framed = Framed::new(stream, LfTerminatedCodec::default());

    let mut accounts: HashMap<Account, Amount> = HashMap::new();

    while let Some(item) = framed.next().await {
        let frame = item?; // BytesMut
        // println!("Received frame: {:?}", frame);

        match frame.action {
            Action::OpenAccount(x) => {
                accounts.insert(x, 0);
            },
            Action::Deposit(x, v) => {accounts.entry(x).and_modify(|e| *e += v);},
            Action::Transfer(f, t, v) => {
                // accounts.entry(f).and_modify(|e| *e -= v);
                // do transfer if balance sufficient, else ignore
                if let Some(balance) = accounts.get_mut(&f) {
                    if *balance >= v {
                        *balance -= v;
                        accounts.entry(t).and_modify(|e| *e += v);
                    }
                }
            },
        }
    }

    println!("Connection closed");
    println!("Final account states: {:?}", accounts);

    let mut acc_sum: u128 = 0;
    for (acc, bal) in accounts.iter() {
        acc_sum += (*acc as u128) * (*bal as u128);
    }
    println!("Final account weighted sum: {}", acc_sum);

    Ok(())
}

#[tokio::main]
async fn main() -> io::Result<()> {
    // Your main function implementation
    let listener = TcpListener::bind("127.0.0.1:7077").await?;

    loop {
        let (socket, _) = listener.accept().await?;
        let c = handle_conn(socket).await;
        if let Err(e) = c {
            eprintln!("Error handling connection: {:?}", e);
        }
    }

}