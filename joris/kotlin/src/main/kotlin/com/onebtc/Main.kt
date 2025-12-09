package com.onebtc

fun main(args: Array<String>) {
    val port = args.firstOrNull()?.toIntOrNull() ?: 7077

    println("1BTC Processor starting on port $port...")

    val ledger = Ledger()
    val resequencer = PrimitiveResequencer(ledger)

    val server = NioServer(port, resequencer) { duration ->
        println("All connections closed.")
        println("Processed ${resequencer.getProcessedCount()} transactions")
        println("Accounts: ${ledger.getAccountCount()}")
        println("Duration: ${duration}ms")
        println("Answer: ${ledger.computeAnswer()}")
    }

    server.start()
    println("Server shut down.")
}
