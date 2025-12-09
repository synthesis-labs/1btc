package com.onebtc

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import java.nio.channels.ServerSocketChannel
import java.nio.channels.SocketChannel

/**
 * Raw NIO server - no Netty overhead.
 * Single-threaded selector loop with zero-copy parsing.
 */
class NioServer(
    private val port: Int,
    private val visitor: TransactionVisitor,
    private val onComplete: (durationMs: Long) -> Unit
) {
    private val selector = Selector.open()
    private val serverChannel = ServerSocketChannel.open()

    private var activeConnections = 0
    private var started = false
    private var startTime = 0L

    fun start() {
        serverChannel.bind(InetSocketAddress(port))
        serverChannel.configureBlocking(false)
        serverChannel.register(selector, SelectionKey.OP_ACCEPT)

        println("Listening on port $port")

        while (true) {
            selector.select()

            val keys = selector.selectedKeys().iterator()
            while (keys.hasNext()) {
                val key = keys.next()
                keys.remove()

                try {
                    when {
                        key.isAcceptable -> accept(key)
                        key.isReadable -> read(key)
                    }
                } catch (e: Exception) {
                    e.printStackTrace()
                    closeConnection(key)
                }
            }

            // Check for completion
            if (started && activeConnections == 0) {
                val duration = System.currentTimeMillis() - startTime
                onComplete(duration)
                break
            }
        }

        serverChannel.close()
        selector.close()
    }

    private fun accept(key: SelectionKey) {
        val client = serverChannel.accept()
        client.configureBlocking(false)
        client.socket().tcpNoDelay = true
        client.socket().receiveBufferSize = 4 * 1024 * 1024

        // Heap buffer so we can access backing array directly - faster than direct buffer for parsing
        val buffer = ByteBuffer.allocate(256 * 1024)
        client.register(selector, SelectionKey.OP_READ, ConnectionState(buffer))

        activeConnections++
        started = true
    }

    private fun read(key: SelectionKey) {
        val client = key.channel() as SocketChannel
        val state = key.attachment() as ConnectionState
        val buf = state.buffer

        val bytesRead = client.read(buf)

        if (bytesRead == -1) {
            closeConnection(key)
            return
        }

        if (bytesRead == 0) return

        // Process complete lines from buffer
        buf.flip()
        processBuffer(state)
        buf.compact()
    }

    private fun processBuffer(state: ConnectionState) {
        val buffer = state.buffer
        val data = state.buffer.array()
        var pos = buffer.position()
        val limit = buffer.limit()

        while (pos < limit) {
            // Find newline using array access
            var newlinePos = pos
            while (newlinePos < limit && data[newlinePos] != NL) {
                newlinePos++
            }

            if (newlinePos >= limit) {
                // No complete line - leave data in buffer
                buffer.position(pos)
                return
            }

            // Record start time on first message
            if (startTime == 0L) {
                startTime = System.currentTimeMillis()
            }

            // Auto-detect format on first line
            if (!state.formatDetected) {
                state.formatDetected = true
                state.isCompactFormat = data[pos] in DIGIT_0..DIGIT_9
            }

            // Parse directly from array - no copy needed
            if (state.isCompactFormat) {
                parseCompact(data, pos, newlinePos)
            } else {
                parseVerbose(data, pos, newlinePos)
            }

            pos = newlinePos + 1
        }
        buffer.position(pos)
    }

    private fun parseCompact(arr: ByteArray, start: Int, end: Int) {
        var pos = start

        // Read sequence number (variable length)
        var seq = 0L
        while (arr[pos] != PIPE) {
            seq = seq * 10 + (arr[pos++] - DIGIT_0)
        }
        pos++ // skip '|'

        when (arr[pos++]) {
            OP_O -> {
                pos++ // skip '|'
                visitor.onOpen(seq, readAccount12(arr, pos))
            }

            OP_D -> {
                pos++ // skip '|'
                var amount = 0L
                while (arr[pos] != PIPE) {
                    amount = amount * 10 + (arr[pos++] - DIGIT_0)
                }
                pos++ // skip '|'
                visitor.onDeposit(seq, amount, readAccount12(arr, pos))
            }

            OP_T -> {
                pos++ // skip '|'
                var amount = 0L
                while (arr[pos] != PIPE) {
                    amount = amount * 10 + (arr[pos++] - DIGIT_0)
                }
                pos++ // skip '|'
                val from = readAccount12(arr, pos)
                pos += 13 // 12 digits + '|'
                visitor.onTransfer(seq, amount, from, readAccount12(arr, pos))
            }
        }
    }

    // Unrolled 12-digit parser: split into 3 groups of 4 digits for better
    // instruction pipelining (each group fits in a register without overflow)
    private fun readAccount12(arr: ByteArray, p: Int): Long {
        val d0 = (arr[p].toLong() - DIGIT_0) * 1000 + (arr[p + 1].toLong() - DIGIT_0) * 100 +
                (arr[p + 2].toLong() - DIGIT_0) * 10 + (arr[p + 3].toLong() - DIGIT_0)
        val d1 = (arr[p + 4].toLong() - DIGIT_0) * 1000 + (arr[p + 5].toLong() - DIGIT_0) * 100 +
                (arr[p + 6].toLong() - DIGIT_0) * 10 + (arr[p + 7].toLong() - DIGIT_0)
        val d2 = (arr[p + 8].toLong() - DIGIT_0) * 1000 + (arr[p + 9].toLong() - DIGIT_0) * 100 +
                (arr[p + 10].toLong() - DIGIT_0) * 10 + (arr[p + 11].toLong() - DIGIT_0)
        return d0 * 100_000_000 + d1 * 10_000 + d2
    }

    private fun parseVerbose(arr: ByteArray, start: Int, end: Int) {
        var pos = start + 18 // skip "Transaction (seq: "

        // Read sequence number
        var seq = 0L
        while (arr[pos] != PAREN) {
            seq = seq * 10 + (arr[pos++] - DIGIT_0)
        }
        pos += 5 // skip ") => "

        when (arr[pos]) {
            OP_O -> {
                pos += 12 // skip "OpenAccount "
                var account = 0L
                while (pos < end) {
                    account = account * 10 + (arr[pos++] - DIGIT_0)
                }
                visitor.onOpen(seq, account)
            }

            OP_D -> {
                pos += 8 // skip "Deposit "
                var amount = 0L
                while (arr[pos] != SPACE) {
                    amount = amount * 10 + (arr[pos++] - DIGIT_0)
                }
                pos += 4 // skip " to "
                var account = 0L
                while (pos < end) {
                    account = account * 10 + (arr[pos++] - DIGIT_0)
                }
                visitor.onDeposit(seq, amount, account)
            }

            OP_T -> {
                pos += 9 // skip "Transfer "
                var amount = 0L
                while (arr[pos] != SPACE) {
                    amount = amount * 10 + (arr[pos++] - DIGIT_0)
                }
                pos += 6 // skip " from "
                var from = 0L
                while (arr[pos] != SPACE) {
                    from = from * 10 + (arr[pos++] - DIGIT_0)
                }
                pos += 4 // skip " to "
                var to = 0L
                while (pos < end) {
                    to = to * 10 + (arr[pos++] - DIGIT_0)
                }
                visitor.onTransfer(seq, amount, from, to)
            }
        }
    }

    private fun closeConnection(key: SelectionKey) {
        key.cancel()
        key.channel().close()
        activeConnections--
    }

    private class ConnectionState(
        val buffer: ByteBuffer,
        var formatDetected: Boolean = false,
        var isCompactFormat: Boolean = true
    )

    companion object {
        private const val NL: Byte = '\n'.code.toByte()
        private const val PIPE: Byte = '|'.code.toByte()
        private const val PAREN: Byte = ')'.code.toByte()
        private const val SPACE: Byte = ' '.code.toByte()
        private const val DIGIT_0: Byte = '0'.code.toByte()
        private const val DIGIT_9: Byte = '9'.code.toByte()
        private const val OP_O: Byte = 'O'.code.toByte()
        private const val OP_D: Byte = 'D'.code.toByte()
        private const val OP_T: Byte = 'T'.code.toByte()
    }
}
