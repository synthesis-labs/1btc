use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::map,
    sequence::{preceded, tuple},
};

use crate::types::{Action, Transaction};

fn parse_u64(input: &str) -> IResult<&str, u64> {
    map(digit1, |s: &str| s.parse().unwrap())(input)
}

fn parse_account(input: &str) -> IResult<&str, String> {
    map(digit1, |s: &str| s.to_string())(input)
}

fn parse_open_account(input: &str) -> IResult<&str, Action> {
    map(preceded(tag("O|"), parse_account), |account| {
        Action::OpenAccount { account }
    })(input)
}

fn parse_deposit(input: &str) -> IResult<&str, Action> {
    map(
        tuple((
            preceded(tag("D|"), parse_u64),
            preceded(tag("|"), parse_account),
        )),
        |(amount, account)| Action::Deposit { account, amount },
    )(input)
}

fn parse_transfer(input: &str) -> IResult<&str, Action> {
    map(
        tuple((
            preceded(tag("T|"), parse_u64),
            preceded(tag("|"), parse_account),
            preceded(tag("|"), parse_account),
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
        tuple((parse_u64, preceded(tag("|"), parse_action))),
        |(seq, action)| Transaction { seq, action },
    )(input)
}

pub fn parse(line: &str) -> Transaction {
    match parse_transaction(line) {
        Ok((_, transaction)) => transaction,
        Err(e) => panic!("Parse error: {}", e),
    }
}
