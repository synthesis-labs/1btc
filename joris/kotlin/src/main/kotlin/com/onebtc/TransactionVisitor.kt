package com.onebtc

/**
 * Visitor interface for processing transactions without heap allocation.
 * All parameters are primitives passed on the stack.
 */
interface TransactionVisitor {
    fun onOpen(seq: Long, account: Long)
    fun onDeposit(seq: Long, amount: Long, account: Long)
    fun onTransfer(seq: Long, amount: Long, from: Long, to: Long)
}
