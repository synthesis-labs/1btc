use crate::types::{Action, Transaction};

pub fn parse(line: &str) -> Transaction {
    let bytes = line.as_bytes();
    let mut pos = 0;

    // Parse sequence number
    let seq_end = bytes.iter().position(|&b| b == b'|').unwrap();
    let seq = parse_u64(&bytes[pos..seq_end]);
    pos = seq_end + 1;

    // Parse action type
    let action_type = bytes[pos];
    pos += 2; // Skip action char and '|'

    let action = match action_type {
        b'O' => {
            // OpenAccount: just account number
            let account = parse_account(&bytes[pos..bytes.len()]);
            Action::OpenAccount { account }
        }
        b'D' => {
            // Deposit: amount|account
            let sep = pos + bytes[pos..].iter().position(|&b| b == b'|').unwrap();
            let amount = parse_u64(&bytes[pos..sep]);
            let account = parse_account(&bytes[sep + 1..bytes.len()]);
            Action::Deposit { account, amount }
        }
        b'T' => {
            // Transfer: amount|from_account|to_account
            let sep1 = pos + bytes[pos..].iter().position(|&b| b == b'|').unwrap();
            let amount = parse_u64(&bytes[pos..sep1]);
            let sep2 = sep1 + 1 + bytes[sep1 + 1..].iter().position(|&b| b == b'|').unwrap();
            let from_account = parse_account(&bytes[sep1 + 1..sep2]);
            let to_account = parse_account(&bytes[sep2 + 1..bytes.len()]);
            Action::Transfer {
                from_account,
                to_account,
                amount,
            }
        }
        _ => panic!("Invalid action type"),
    };

    Transaction { seq, action }
}

#[inline]
fn parse_u64(bytes: &[u8]) -> u64 {
    let mut result = 0u64;
    for &byte in bytes {
        result = result * 10 + (byte - b'0') as u64;
    }
    result
}

#[inline]
fn parse_account(bytes: &[u8]) -> String {
    unsafe { String::from_utf8_unchecked(bytes.to_vec()) }
}
