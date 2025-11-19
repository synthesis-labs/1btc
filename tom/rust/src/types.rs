#[derive(Debug)]
pub struct Transaction {
    pub seq: u64,
    pub action: Action,
}

#[derive(Debug)]
pub enum Action {
    OpenAccount { account: String },
    Deposit { account: String, amount: u64 },
    Transfer { from_account: String, to_account: String, amount: u64 },
}
