{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import           Control.Exception      (bracket)
import           Control.Monad          (forever, (<$!>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT, evalState, evalStateT, get,
                                         lift, modify, modify')
import qualified Data.Map.Strict        as Map
import qualified Data.Text.IO           as TIO
import           GHC.IO.Handle          (Handle)
import           Network.Socket
import           System.IO              (IOMode (ReadMode), hClose, hIsEOF)

import           Control.Concurrent     (modifyMVar_, newMVar, putMVar,
                                         readMVar, withMVar)
import           Control.Monad.Loops
import           Debug.Trace            (trace)
import           ParserAtto             (parseAtto, transactionAtto)
import           ParserMega             (parseMega, transactionMega)
import           Types

type Accounts = (Map.Map Account Amount)

-- Process function which doesn't allow negative account balances
process :: Transaction -> Accounts -> Accounts
process (Transaction _seq (OpenAccount account)) = Map.insert account 0
process (Transaction _seq (Deposit amount account)) = Map.update (Just . (+) amount) account
process (Transaction _seq (Transfer amount fromAccount toAccount)) = \accounts ->
    let fromBal = Map.lookup fromAccount accounts
     in if fromBal >= Just amount
            then Map.update (Just . (+) amount) toAccount
               $ Map.update (Just . flip (-) amount) fromAccount accounts
            -- Insufficient funds
            else accounts

-- Process function which does allow negative account balances ("overdrawn")
processWithOverdraft :: Transaction -> Accounts -> Accounts
processWithOverdraft (Transaction _seq (OpenAccount account)) = Map.insert account 0
processWithOverdraft (Transaction _seq (Deposit amount account)) = Map.update (Just . (+) amount) account
processWithOverdraft (Transaction _seq (Transfer amount fromAccount toAccount)) =
    Map.update (Just . (+) amount) toAccount . Map.update (Just . flip (-) amount) fromAccount

-- Using an MVar to keep state
processAllUsingMVar :: Handle -> IO Accounts
processAllUsingMVar handle = do
    accountsRef <- newMVar Map.empty

    -- Loop and update the Map
    -- This runs in constant memory
    untilM_
        (do
            line <- TIO.hGetLine handle
            packet <- parseMega transactionMega line
            -- mvars are lazy so we force evaluation here with $!
            modifyMVar_ accountsRef (\accounts -> pure $! processWithOverdraft packet accounts)
        )
        (hIsEOF handle)

    readMVar accountsRef

-- Using StateT to keep state
processAllUsingState :: Handle -> IO Accounts
processAllUsingState handle = evalStateT go Map.empty
    where
        go :: StateT Accounts IO Accounts
        go = do
            untilM_
                (do
                    line <- liftIO $ TIO.hGetLine handle
                    packet <- parseMega transactionMega line
                    -- modify' is the strict version
                    modify' $ processWithOverdraft packet
                )
                (liftIO $ hIsEOF handle)
            get

main :: IO ()
main = do
    sock <- socket AF_INET Stream defaultProtocol
    let addr = SockAddrInet 7077 (tupleToHostAddress (127, 0, 0, 1))
    bind sock addr
    listen sock 1
    putStrLn "Listening on port 7077..."
    forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Client connected from: " ++ show peer

        -- Use the main thread for now
        results <- bracket
                        (socketToHandle conn ReadMode)
                        hClose
                        processAllUsingState

        putStrLn $ "Results: " <> show (sum $ Map.elems results)
        putStrLn "Done"
        pure ()




