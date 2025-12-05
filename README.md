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

## Compact mode

For the crazies there is also a compact wire protocol which reduces the number of bytes on the wire, by removing superflous bits of each message, and seperating each field with a `|` - turn it on with `--compact` flag to the generator:

```
> cargo run --release -- --num-accounts 2 --num-transfers 2 --compact --output-stdout
0|O|375339063135
1|D|16330129|375339063135
2|O|894804063960
3|D|2245613|894804063960
4|T|647548|894804063960|375339063135
5|T|208011|375339063135|894804063960
```

## Concurrency

If you're ready to take things to the next level then `--connections` and `--batch` are your new friends! 

- `--connections` -> number of concurrent processes to run to generate transactions
- `--batch` -> number of sequential transactions per process to generate in a round-robin fashion

e.g. `--connections 4 --batch 1` will generate: 
- `connection 0 -> [  0,  4,  8, 12, ..]`
- `connection 1 -> [  1,  5,  9, 13, ..]`
- `connection 2 -> [  2,  6, 10, 14, ..]`
- `connection 3 -> [  3,  7, 11, 15, ..]`

while `--connections 4 --batch 4` will generate:
- `connection 0 -> [  0,  1,  2,  3, ..]`
- `connection 1 -> [  4,  5,  6,  7, ..]`
- `connection 2 -> [  8,  9, 10, 11, ..]`
- `connection 3 -> [ 12, 13, 14, 15, ..]`

## Known good answers

We track a number of known-good answers for testing purposes:

| Num Accounts (`--num-accounts`) | Num Transfers (`--num-transfers`) | Value |
| --- | --- | --- |
|       2 |             1 | 26789356497795734557 |
|      10 |            10 | 166457144743206452491 |
|   1,000 |         1,000 | 14843982935862577942349 |
|   1,000 |        10,000 | 14844401297437455612815 |
| 200,000 |             1 | 2745017987012080295698533 |
| 200,000 |            10 | 2745018235689397693183495 |
| 200,000 |           100 | 2745016059864085133436106 |
| 200,000 |         1,000 | 2745011554539198564613888 |
| 200,000 |        10,000 | 2745016940405660348538023 |
| 200,000 |       100,000 | 2745027965563243583821811 |
| 200,000 |     1,000,000 | 2744936786565869061234119 |
| 200,000 |    10,000,000 | 2744526020641388560472069 |
| 200,000 |   100,000,000 | 2742816079156645111811128 |
| 200,000 | 1,000,000,000 | 2736024334292708155522530 |

## Results

We will keep track of everyone's scores as we write solutions.

| folder | language | notes | time |
| --- | --- | --- | --- |
| tom/ | haskell | `singleThreaded` - Very basic, single-threaded, StateT/MVar, not optimised | ~ 38 minutes |
| mike/ | rust | `simple parser` - basic solution to validate logic, not optimised | ~ 35 minutes |
| tom/ | haskell | `simpleThreaded` - Basic, split read, parse and process into seperate threads (to keep ordering), TVar updates | ~ 28 minutes |
| tom/ | haskell | `queuedStm` - Using STM to coordinate updates to accounts, using a thread pool. Ordering completely screwed of course - so incorrect results. | ~ 18 minutes |
| mike/ | rust | `optimised parser` - Applied some brains to the parser using a flamegraph to identify bottlenecks | ~ 2 minutes |
| mike/ | rust | `unsafe strings` - convert stream to Str using unchecked method | ~ 40.57 seconds |
| tom/ | rust | `batch_reorder` - Multiple producers receiving and parsing with a single consumer maintaining balances. Run with `--connections 12 --batch 100000 --compact` | ~ 12.4 seconds |

## Contributing

This project is very much in its infancy, and really needs peoples ideas and contributions to work. Please feel free to open up an issue (for ideas) or PR to contribute to solutions or any other part of the repo.