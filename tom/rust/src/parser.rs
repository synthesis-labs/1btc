use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{digit1, space1},
    combinator::map,
    sequence::{preceded, tuple},
    IResult,
};

use crate::types::{Action, Transaction};

fn parse_number_string(input: &str) -> IResult<&str, String> {
    map(digit1, |s: &str| s.to_string())(input)
}

fn parse_u64(input: &str) -> IResult<&str, u64> {
    map(digit1, |s: &str| s.parse().unwrap())(input)
}

fn parse_open_account(input: &str) -> IResult<&str, Action> {
    map(
        preceded(tag("OpenAccount "), parse_number_string),
        |account| Action::OpenAccount { account },
    )(input)
}

fn parse_deposit(input: &str) -> IResult<&str, Action> {
    map(
        tuple((
            preceded(tag("Deposit "), parse_u64),
            preceded(tag(" to "), parse_number_string),
        )),
        |(amount, account)| Action::Deposit { account, amount },
    )(input)
}

fn parse_transfer(input: &str) -> IResult<&str, Action> {
    map(
        tuple((
            preceded(tag("Transfer "), parse_u64),
            preceded(tag(" from "), parse_number_string),
            preceded(tag(" to "), parse_number_string),
        )),
        |(amount, from_account, to_account)| Action::Transfer {
            from_account,
            to_account,
            amount,
        },
    )(input)
}

fn parse_action(input: &str) -> IResult<&str, Action> {
    alt((parse_transfer, parse_deposit, parse_open_account))(input)
}

fn parse_transaction(input: &str) -> IResult<&str, Transaction> {
    map(
        tuple((
            preceded(tag("Transaction (seq: "), parse_u64),
            preceded(tag(") => "), parse_action),
        )),
        |(seq, action)| Transaction { seq, action },
    )(input)
}

pub fn parse_line(line: &str) -> Result<Transaction, String> {
    match parse_transaction(line) {
        Ok((_, transaction)) => Ok(transaction),
        Err(e) => Err(format!("Parse error: {}", e)),
    }
}
