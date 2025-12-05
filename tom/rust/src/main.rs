use dashmap::DashMap;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, LazyLock};
use std::time::Instant;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::mpsc;

const BATCH_SIZE: usize = 10_000;

static BATCHES: LazyLock<DashMap<u64, Vec<Transaction>>> = LazyLock::new(DashMap::new);
static TOTAL_SUBMITTED: AtomicU64 = AtomicU64::new(0);
static START_TIME: LazyLock<std::sync::Mutex<Option<Instant>>> =
    LazyLock::new(|| std::sync::Mutex::new(None));

struct Transaction {
    seq: u64,
    action: Action,
}

enum Action {
    Open { account: u64 },
    Deposit { account: u64, amount: u32 },
    Transfer { from: u64, to: u64, amount: u32 },
}

fn parse(line: &str) -> Transaction {
    let b = line.as_bytes();
    let p1 = b.iter().position(|&c| c == b'|').unwrap();
    let seq = parse_u64(&b[..p1]);
    let action = match b[p1 + 1] {
        b'O' => Action::Open {
            account: parse_u64(&b[p1 + 3..]),
        },
        b'D' => {
            let p2 = p1 + 3 + b[p1 + 3..].iter().position(|&c| c == b'|').unwrap();
            Action::Deposit {
                amount: parse_u32(&b[p1 + 3..p2]),
                account: parse_u64(&b[p2 + 1..]),
            }
        }
        b'T' => {
            let p2 = p1 + 3 + b[p1 + 3..].iter().position(|&c| c == b'|').unwrap();
            let p3 = p2 + 1 + b[p2 + 1..].iter().position(|&c| c == b'|').unwrap();
            Action::Transfer {
                amount: parse_u32(&b[p1 + 3..p2]),
                from: parse_u64(&b[p2 + 1..p3]),
                to: parse_u64(&b[p3 + 1..]),
            }
        }
        _ => panic!("invalid action"),
    };
    Transaction { seq, action }
}

#[inline]
fn parse_u64(b: &[u8]) -> u64 {
    b.iter().fold(0, |acc, &c| acc * 10 + (c - b'0') as u64)
}

#[inline]
fn parse_u32(b: &[u8]) -> u32 {
    b.iter().fold(0, |acc, &c| acc * 10 + (c - b'0') as u32)
}

async fn handle_connection(socket: TcpStream, notify: mpsc::UnboundedSender<()>) {
    let reader = BufReader::new(socket);
    let mut lines = reader.lines();
    let mut batch = Vec::with_capacity(BATCH_SIZE);

    while let Ok(Some(line)) = lines.next_line().await {
        batch.push(parse(&line));
        if batch.len() >= BATCH_SIZE {
            let seq = batch[0].seq;
            let len = batch.len() as u64;
            BATCHES.insert(
                seq,
                std::mem::replace(&mut batch, Vec::with_capacity(BATCH_SIZE)),
            );
            TOTAL_SUBMITTED.fetch_add(len, Ordering::Relaxed);
            let _ = notify.send(());
        }
    }

    if !batch.is_empty() {
        let seq = batch[0].seq;
        let len = batch.len() as u64;
        BATCHES.insert(seq, batch);
        TOTAL_SUBMITTED.fetch_add(len, Ordering::Relaxed);
        let _ = notify.send(());
    }
}

fn process_batch(txs: Vec<Transaction>, accounts: &mut HashMap<u64, u32>) {
    for tx in txs {
        match tx.action {
            Action::Open { account } => {
                accounts.insert(account, 0);
            }
            Action::Deposit { account, amount } => {
                *accounts.entry(account).or_insert(0) += amount;
            }
            Action::Transfer { from, to, amount } => {
                if let Some(bal) = accounts.get_mut(&from) {
                    if *bal >= amount {
                        *bal -= amount;
                        *accounts.entry(to).or_insert(0) += amount;
                    }
                }
            }
        }
    }
}

fn consumer(mut rx: mpsc::UnboundedReceiver<()>, done: Arc<AtomicUsize>) {
    let mut next_seq = 0u64;
    let mut accounts: HashMap<u64, u32> = HashMap::with_capacity(100_000);
    let mut batch_count = 0u64;

    loop {
        // Process all available batches in sequence
        while let Some((_, txs)) = BATCHES.remove(&next_seq) {
            next_seq += txs.len() as u64;
            process_batch(txs, &mut accounts);
            batch_count += 1;
            if batch_count % 10000 == 0 {
                println!("Processed {} batches ({} txs)", batch_count, next_seq);
            }
        }

        // Check if all connections are done and we've processed everything
        let is_done = done.load(Ordering::Relaxed) == 1;
        let total = TOTAL_SUBMITTED.load(Ordering::Relaxed);
        if is_done && next_seq >= total {
            break;
        }

        // Wait for notification
        match rx.blocking_recv() {
            Some(()) => {}
            None => break,
        }
    }

    let weighted_sum: u128 = accounts.iter().map(|(&k, &v)| k as u128 * v as u128).sum();
    println!("\nAccounts: {}", accounts.len());
    println!("Transactions: {}", next_seq);
    println!("Weighted sum: {}", weighted_sum);
    if let Some(t) = *START_TIME.lock().unwrap() {
        println!("Total time: {:.2}s", t.elapsed().as_secs_f64());
    }
    std::process::exit(0);
}

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let (notify_tx, notify_rx) = mpsc::unbounded_channel();
    let done = Arc::new(AtomicUsize::new(0));

    let done_for_consumer = done.clone();
    std::thread::spawn(move || consumer(notify_rx, done_for_consumer));

    let listener = TcpListener::bind("127.0.0.1:7077").await?;
    println!("Listening on 127.0.0.1:7077");

    let active = Arc::new(AtomicUsize::new(0));

    loop {
        let (socket, _) = listener.accept().await?;

        if active.load(Ordering::Relaxed) == 0 {
            *START_TIME.lock().unwrap() = Some(Instant::now());
            println!("Timer started");
        }

        let conn_id = active.fetch_add(1, Ordering::Relaxed);
        println!("Connection {} opened", conn_id);

        let notify = notify_tx.clone();
        let active = active.clone();
        let done = done.clone();

        tokio::spawn(async move {
            handle_connection(socket, notify.clone()).await;
            println!("Connection {} closed", conn_id);

            if active.fetch_sub(1, Ordering::Relaxed) == 1 {
                println!("\nAll connections closed");
                if let Some(t) = *START_TIME.lock().unwrap() {
                    println!("Receive time: {:.2}s", t.elapsed().as_secs_f64());
                }
                done.store(1, Ordering::Relaxed);
                // Wake up consumer to check done flag
                let _ = notify.send(());
            }
        });
    }
}
