# 1 Billion Payments Challenge!

Can you process 1 billion payments between to 100k accounts? Quickly? Give it a go!

The challenge is to track each transaction and keep an accurate set of account balances for each of the accounts. The only rule is that transactions should be processed in order (per account), and that accounts cannot go into negative balances (you should ignore those transactions that attempt to).

Write an app that listens on TCP socket 7077, and listens for three types of messages:

```
Transaction (seq: <sequence>) => OpenAccount <account>
Transaction (seq: <sequence>) => Deposit <amount> to <account>
Transaction (seq: <sequence>) => Transfer <amount> from <account> to <account>
```

- `<sequence>` will be a monotomically incrementing number (starting at 0).
- `<account>` is a string value of length 12 e.g. "894804063960"
- `<amount>` is a string value of variable length, representing a value in cents e.g. "647548" means 6475.48 (in whatever currency you like!)

## Running the generator

The transaction generator is a command-line tool, which will connect to your socket (port 7077), and then start sending transactions. By default it will open 100,000 accounts, depositing a balance into each, and then deliver 1,000,000,000 transfer messages sending money between those accounts.

However for testing purposes you can change these parameters using `--num-transfers` and `--num-accounts`.

You will need to have Rust installed:

```
> cargo run --release                                           # Run the full 1 billion
> cargo run --release -- --num-accounts 2 --num-transfers 2     # A small test set
```

## Results

We will keep track of everyone's scores as we write solutions.

| folder | language | notes | time |
| --- | --- | --- | --- |
| tom/ | haskell | Very basic, single-threaded, StateT/MVar, not optimised | ~ 70 minutes |