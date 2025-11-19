```
➜  generator git:(main) ✗ date && cargo run --release  && date         
Tue 18 Nov 2025 15:03:16 +03
Generating transactions and firing them to 127.0.0.1:7077
Accounts sent!
  Sent 10000000 total messages...
  Sent 20000000 total messages...
  Sent 30000000 total messages...

  ...

  Sent 970000000 total messages...
  Sent 980000000 total messages...
  Sent 990000000 total messages...
  Sent 1000000000 total messages...
Done! (1000200000 total messages)
Tue 18 Nov 2025 15:05:28 +03
```

```
cargo run --release  121,02s user 11,67s system 99% cpu 2:12,85 total
cargo run --release  119,75s user 12,61s system 99% cpu 2:12,61 total
```

Now with improved generator:
```
cargo run --release  40,57s user 5,54s system 53% cpu 1:26,39 total
```


With output:

```
Final account weighted sum: 13786738411xxxxxxxxxx1623
```