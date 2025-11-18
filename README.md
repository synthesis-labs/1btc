# 1 Billion TRANSACTIONS Challenge!

Can you process 1 billion payments between to 100k accounts? Quickly? Give it a go!

The challenge is to track each transaction and keep an accurate set of account balances for each of the accounts. The only rule is that transactions should be processed in order (per account), and that accounts cannot go into negative balances (you should ignore those transactions that attempt to).

Write an app that listens on TCP socket 7077, and listens for three types of messages:

```
Transaction (seq: <sequence>) => OpenAccount <account>
Transaction (seq: <sequence>) => Deposit <amount> to <account>
Transaction (seq: <sequence>) => Transfer <amount> from <account> to <account>
```

- `<sequence>` will be a monotomically incrementing number (starting at 0).
- `<account>` is a string value of length 12 e.g. `894804063960`
- `<amount>` is a string value of variable length, representing a value in cents e.g. `647548` means `6475.48` (in whatever currency you like!)

Your final result should be a total of the `<account>` multiplied by the `<amount>` as a single large integer value, i.e.:

```
in pseudocode:
answer := accounts.map((account, balance) => account * balance).sum()
```

## Running the generator

The transaction generator is a command-line tool, which will connect to your socket (port 7077), and then start sending transactions. By default it will open 100,000 accounts, depositing a balance into each, and then deliver 1,000,000,000 transfer messages sending money between those accounts.

However for testing purposes you can change these parameters using `--num-transfers` and `--num-accounts`.

You will need to have Rust installed:

```
> cargo run --release                                           # Run the full 1 billion
> cargo run --release -- --num-accounts 2 --num-transfers 2     # A small test set
```

## Known good answers

We track a number of known-good answers for testing purposes:

| Num Accounts (`--num-accounts`) | Num Transfers (`--num-transfers`) | Value |
| --- | --- | --- |
|       2 |             1 | 7802340435860874795 |
|      10 |            10 | 206922370281506044649 |
|   1,000 |         1,000 | 14586923064889399786004 |
|   1,000 |        10,000 | 14604034398384103320717 |
| 200,000 |             1 | 1379473486286508022766389 |
| 200,000 |            10 | 1379473157038380158919351 |
| 200,000 |           100 | 1379473030790083308728760 |
| 200,000 |         1,000 | 1379469252337716689591052 |
| 200,000 |        10,000 | 1379448067965350643940280 |
| 200,000 |       100,000 | 1379474499842954888781766 |
| 200,000 |     1,000,000 | 1379547085520990531072141 |
| 200,000 |    10,000,000 | 1379887199356639263280479 |
| 200,000 |   100,000,000 | 1379236686063303335021910 |
| 200,000 | 1,000,000,000 | 1378673841163113785331623 |

## Results

We will keep track of everyone's scores as we write solutions.

| folder | language | notes | time |
| --- | --- | --- | --- |
| tom/ | haskell | Very basic, single-threaded, StateT/MVar, not optimised | ~ 70 minutes |

## Contributing

This project is very much in its infancy, and really needs peoples ideas and contributions to work. Please feel free to open up an issue (for ideas) or PR to contribute to solutions or any other part of the repo.