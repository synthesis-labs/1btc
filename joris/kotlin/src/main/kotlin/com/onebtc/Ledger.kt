package com.onebtc

import org.eclipse.collections.impl.map.mutable.primitive.LongLongHashMap
import java.math.BigInteger

/**
 * Account ledger using primitive collections to avoid boxing overhead.
 * Implements the business logic for the 1BTC challenge.
 */
class Ledger : TransactionVisitor {

    private val balances = LongLongHashMap(200_000) // expected number of accounts

    override fun onOpen(seq: Long, account: Long) {
        balances.put(account, 0L)
    }

    override fun onDeposit(seq: Long, amount: Long, account: Long) {
        balances.addToValue(account, amount)
    }

    override fun onTransfer(seq: Long, amount: Long, from: Long, to: Long) {
        var transferApproved = false
        balances.updateValue(from, 0L) { balance ->
            if (balance >= amount) {
                transferApproved = true
                balance - amount
            } else balance
        }
        if (transferApproved) {
            balances.addToValue(to, amount)
        }
    }

    /**
     * Compute the final answer: sum of (account_id * balance) for all accounts.
     */
    fun computeAnswer(): BigInteger {
        var sum = BigInteger.ZERO
        balances.forEachKeyValue { account, balance ->
            sum += BigInteger.valueOf(account) * BigInteger.valueOf(balance)
        }
        return sum
    }

    fun getAccountCount(): Int = balances.size()
}
