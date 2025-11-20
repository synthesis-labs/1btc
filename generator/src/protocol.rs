use itoa::Buffer;

pub fn write_account(buf: &mut String, account: &u64) {
    let mut itoa_buf = itoa::Buffer::new();
    let account_str = itoa_buf.format(*account);
    let padding = 12 - account_str.len();
    for _ in 0..padding {
        buf.push('0');
    }
    buf.push_str(account_str);
}

pub fn write_open_account(
    compact: bool,
    msg_buf: &mut String,
    itoa_buf: &mut Buffer,
    seq: &u32,
    account: &u64,
) {
    msg_buf.clear();
    if compact {
        // 0|O|375339063135
        msg_buf.push_str(itoa_buf.format(*seq));
        msg_buf.push_str("|O|");
        write_account(msg_buf, account);
    } else {
        // Transaction (seq: 0) => OpenAccount 375339063135
        msg_buf.push_str("Transaction (seq: ");
        msg_buf.push_str(itoa_buf.format(*seq));
        msg_buf.push_str(") => OpenAccount ");
        write_account(msg_buf, account);
    }
    msg_buf.push('\n');
}

pub fn write_deposit(
    compact: bool,
    msg_buf: &mut String,
    itoa_buf: &mut Buffer,
    seq: &u32,
    account: &u64,
    amount: &u32,
) {
    msg_buf.clear();
    if compact {
        // 1|D|16330129|375339063135
        msg_buf.push_str(itoa_buf.format(*seq));
        msg_buf.push_str("|D|");
        msg_buf.push_str(itoa_buf.format(*amount));
        msg_buf.push_str("|");
        write_account(msg_buf, account);
    } else {
        // Transaction (seq: 1) => Deposit 16330129 to 375339063135
        msg_buf.push_str("Transaction (seq: ");
        msg_buf.push_str(itoa_buf.format(*seq));
        msg_buf.push_str(") => Deposit ");
        msg_buf.push_str(itoa_buf.format(*amount));
        msg_buf.push_str(" to ");
        write_account(msg_buf, account);
    }
    msg_buf.push('\n');
}

pub fn write_transfer(
    compact: bool,
    msg_buf: &mut String,
    itoa_buf: &mut Buffer,
    seq: &u32,
    from_account: &u64,
    to_account: &u64,
    amount: &u32,
) {
    msg_buf.clear();

    if compact {
        // 4|T|647548|894804063960|375339063135
        msg_buf.push_str(itoa_buf.format(*seq));
        msg_buf.push_str("|T|");
        msg_buf.push_str(itoa_buf.format(*amount));
        msg_buf.push_str("|");
        write_account(msg_buf, from_account);
        msg_buf.push_str("|");
        write_account(msg_buf, to_account);
    } else {
        // Transaction (seq: 4) => Transfer 647548 from 894804063960 to 375339063135
        msg_buf.push_str("Transaction (seq: ");
        msg_buf.push_str(itoa_buf.format(*seq));
        msg_buf.push_str(") => Transfer ");
        msg_buf.push_str(itoa_buf.format(*amount));
        msg_buf.push_str(" from ");
        write_account(msg_buf, from_account);
        msg_buf.push_str(" to ");
        write_account(msg_buf, to_account);
    }
    msg_buf.push('\n');
}
