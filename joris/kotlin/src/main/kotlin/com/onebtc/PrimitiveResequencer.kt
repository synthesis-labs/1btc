package com.onebtc

/**
 * Resequencer using parallel primitive arrays to avoid heap allocation.
 * Buffers out-of-order arrivals and drains in sequence order.
 * Grows dynamically when buffer capacity is exceeded.
 *
 * Capacity must be a power of 2 for efficient masking.
 */
class PrimitiveResequencer(
    private val visitor: TransactionVisitor,
    initialCapacity: Int = 2097152 // 2^21, around 2M slots
) : TransactionVisitor {

    private var capacity = initialCapacity
    private var mask = (capacity - 1).toLong()

    // Parallel arrays for buffered transactions (struct-of-arrays for cache efficiency)
    private var sequenceIDs = LongArray(capacity)    // for collision detection
    private var types = ByteArray(capacity)          // O, D, or T
    private var amounts = LongArray(capacity)        // transfer/deposit amount
    private var debitAccounts = LongArray(capacity)  // source account (or account for open/deposit)
    private var creditAccounts = LongArray(capacity) // destination account (transfers only)
    private var present = BooleanArray(capacity)     // slot occupancy

    private var expectedSeq = 0L

    private fun index(seq: Long): Int = (seq and mask).toInt()

    private fun store(seq: Long, type: Byte, amount: Long, from: Long, to: Long) {
        val idx = index(seq)

        // Check for collision - slot occupied by a different sequence
        if (present[idx] && sequenceIDs[idx] != seq) {
            grow()
            store(seq, type, amount, from, to)
            return
        }

        sequenceIDs[idx] = seq
        types[idx] = type
        amounts[idx] = amount
        debitAccounts[idx] = from
        creditAccounts[idx] = to
        present[idx] = true
    }

    private fun grow() {
        println("Growing resequencer capacity from $capacity to ${2 * capacity}")
        val oldCapacity = capacity
        val oldSeqs = sequenceIDs
        val oldTypes = types
        val oldAmounts = amounts
        val oldDebitAccounts = debitAccounts
        val oldCreditAccounts = creditAccounts
        val oldPresent = present

        capacity *= 2
        mask = (capacity - 1).toLong()

        sequenceIDs = LongArray(capacity)
        types = ByteArray(capacity)
        amounts = LongArray(capacity)
        debitAccounts = LongArray(capacity)
        creditAccounts = LongArray(capacity)
        present = BooleanArray(capacity)

        // Rehash all existing entries
        for (i in 0 until oldCapacity) {
            if (oldPresent[i]) {
                val newIdx = index(oldSeqs[i])
                sequenceIDs[newIdx] = oldSeqs[i]
                types[newIdx] = oldTypes[i]
                amounts[newIdx] = oldAmounts[i]
                debitAccounts[newIdx] = oldDebitAccounts[i]
                creditAccounts[newIdx] = oldCreditAccounts[i]
                present[newIdx] = true
            }
        }
    }

    override fun onOpen(seq: Long, account: Long) {
        if (seq == expectedSeq) {
            visitor.onOpen(seq, account)
            expectedSeq++
            drain()
        } else {
            store(seq, TYPE_OPEN, 0, account, 0)
        }
    }

    override fun onDeposit(seq: Long, amount: Long, account: Long) {
        if (seq == expectedSeq) {
            visitor.onDeposit(seq, amount, account)
            expectedSeq++
            drain()
        } else {
            store(seq, TYPE_DEPOSIT, amount, account, 0)
        }
    }

    override fun onTransfer(seq: Long, amount: Long, from: Long, to: Long) {
        if (seq == expectedSeq) {
            visitor.onTransfer(seq, amount, from, to)
            expectedSeq++
            drain()
        } else {
            store(seq, TYPE_TRANSFER, amount, from, to)
        }
    }

    private fun drain() {
        var idx = index(expectedSeq)
        while (present[idx] && sequenceIDs[idx] == expectedSeq) {
            present[idx] = false
            when (types[idx]) {
                TYPE_OPEN -> visitor.onOpen(expectedSeq, debitAccounts[idx])
                TYPE_DEPOSIT -> visitor.onDeposit(expectedSeq, amounts[idx], debitAccounts[idx])
                TYPE_TRANSFER -> visitor.onTransfer(expectedSeq, amounts[idx], debitAccounts[idx], creditAccounts[idx])
            }
            expectedSeq++
            idx = index(expectedSeq)
        }
    }

    fun getProcessedCount(): Long = expectedSeq

    companion object {
        private const val TYPE_OPEN: Byte = 'O'.code.toByte()
        private const val TYPE_DEPOSIT: Byte = 'D'.code.toByte()
        private const val TYPE_TRANSFER: Byte = 'T'.code.toByte()
    }
}
