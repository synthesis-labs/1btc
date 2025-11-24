{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import           Control.Exception              (bracket)
import           Control.Monad                  (foldM, forM, forM_, forever,
                                                 replicateM, unless, when,
                                                 (<$!>))
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.State            (StateT, evalState, evalStateT,
                                                 get, lift, modify, modify')
import qualified Data.ByteString.Char8          as BS
import qualified Data.Map.Strict                as Map
import qualified Data.Text.IO                   as TIO
import           GHC.IO.Handle                  (Handle)
import           Network.Socket
import           System.IO                      (BufferMode (BlockBuffering),
                                                 IOMode (ReadMode), hClose,
                                                 hIsEOF, hSetBuffering)

import           Control.Concurrent             (forkFinally, forkIO,
                                                 modifyMVar_, newMVar, putMVar,
                                                 readMVar, withMVar)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TQueue
import           Control.Exception              (catch)
import           Control.Exception.Base         (IOException)
import           Control.Monad.Loops
import           Data.Function                  (fix)
import           Data.Text                      (Text, unpack)
import           Network.Socket.ByteString      (recv)
import           ParserAtto                     (parseAtto, transactionAtto)
import           ParserMega                     (parseMega, transactionMega)
import           Types

type Accounts = Map.Map Account Amount

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

processWithTVar :: Transaction -> TVar (Map.Map Account (TVar Amount)) -> STM ()
processWithTVar (Transaction _seq (OpenAccount account)) accounts = do
    newBalance <- newTVar 0
    modifyTVar' accounts $ Map.insert account newBalance
processWithTVar (Transaction _seq (Deposit amount account)) accounts = do
    accs :: Map.Map Account (TVar Amount) <- readTVar accounts
    case Map.lookup account accs of
        Just balance -> modifyTVar' balance (+ amount)
        Nothing      -> pure ()
processWithTVar (Transaction _seq (Transfer amount fromAccount toAccount)) accountsVar = do
    accounts :: Map.Map Account (TVar Amount) <- readTVar accountsVar
    case (Map.lookup fromAccount accounts, Map.lookup toAccount accounts) of
        (Just fromBalanceVar, Just toBalanceVar) -> do
            fromBalance <- readTVar fromBalanceVar
            when (fromBalance >= amount) $ do
                modifyTVar' fromBalanceVar (flip (-) amount)
                modifyTVar' toBalanceVar (+ amount)
        _ -> pure ()

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
            modifyMVar_ accountsRef (\accounts -> pure $! process packet accounts)
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
                    line <- liftIO $ BS.hGetLine handle
                    packet <- parseAtto transactionAtto line
                    -- modify' is the strict version
                    modify' $ process packet
                )
                (liftIO $ hIsEOF handle)
            get

-- Totally simple, 1 thread
singleThreaded :: Socket -> IO ()
singleThreaded sock =
    forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Client connected from: " ++ show peer

        -- Use the main thread for now
        results <- bracket
                        (socketToHandle conn ReadMode)
                        hClose
                        processAllUsingState

        -- Calculate the final answer (account x balance) for each account
        let r = sum $ (\(acc, bal) -> ((read $ unpack acc) :: Integer) * bal) <$> Map.toList results
        putStrLn $ "Result: " <> show r
        putStrLn "Done"
        pure ()

-- Simple, but with a different threads for parsing vs processing
simpleThreaded :: Socket -> IO ()
simpleThreaded sock =
    forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Client connected from: " ++ show peer

        rawQ :: TBQueue (Maybe [BS.ByteString]) <- newTBQueueIO 1000
        parsedQ :: TBQueue (Maybe [Transaction]) <- newTBQueueIO 1000

        handle <- socketToHandle conn ReadMode

        -- Use a thread for socket -> rawQ
        _ <- forkIO $ bracket
                (pure ())
                (\_ -> do
                    atomically $ writeTBQueue rawQ Nothing
                )
                (\_ -> do
                    hSetBuffering handle (BlockBuffering (Just 65536))
                    untilM_
                        (do
                            lines <- catch
                                        (replicateM 1000 (BS.hGetLine handle))
                                        (\(_ :: IOException) -> pure [])
                            atomically $ writeTBQueue rawQ (Just lines)
                        )
                        (hIsEOF handle)
                )

        -- Another thread to parse lines rawQ -> parsedQ
        _ <- forkIO $ fix (\rec -> do
                    tx <- atomically $ readTBQueue rawQ
                    case tx of
                        Just tx' -> do
                            packets <- mapM (parseAtto transactionAtto) tx'
                            atomically $ writeTBQueue parsedQ (Just packets)
                            rec
                        Nothing -> do
                            atomically $ writeTBQueue parsedQ Nothing
                            pure () -- exit once done
            )

        -- Main thread to process the transactions from parsedQ
        accountsRef <- newMVar Map.empty
        _ <- fix (\rec -> do
                tx <- atomically $ readTBQueue parsedQ
                case tx of
                    Just tx' -> do
                        mapM_ (\tx'' -> modifyMVar_ accountsRef (\accounts -> pure $! process tx'' accounts)) tx'
                        rec
                    Nothing -> pure ()
            )

        results <- readMVar accountsRef

        -- Calculate the final answer (account x balance) for each account
        let r = sum $ (\(acc, bal) -> ((read $ unpack acc) :: Integer) * bal) <$> Map.toList results
        putStrLn $ "Result: " <> show r
        putStrLn "Done"

        hClose handle
        pure ()

-- Totally parallel processing - but screws up account balances
stmQueued :: Socket -> IO ()
stmQueued sock = do
    (conn, peer) <- accept sock
    putStrLn $ "Client connected from: " ++ show peer

    let numWorkers = 12
    let queueDepth = 200000

    queue :: TBQueue (Maybe BS.ByteString) <- newTBQueueIO queueDepth
    accounts :: TVar (Map.Map Account (TVar Amount)) <- newTVarIO Map.empty
    workers :: TVar (Map.Map Int (Maybe ())) <- newTVarIO Map.empty

    forM_ [1..numWorkers] $ \w -> do
        atomically $ modifyTVar' workers (Map.insert w (Just ()))
        forkFinally
            (worker w queue accounts)
            (const $ atomically $ modifyTVar' workers (Map.delete w))

    -- Use the main thread for writing lines to the queue
    bracket
        (socketToHandle conn ReadMode) hClose
        (\handle -> do
            hSetBuffering handle (BlockBuffering (Just 65536))
            untilM_
                (do
                    line <- BS.hGetLine handle
                    atomically $ writeTBQueue queue (Just line)
                )
                (hIsEOF handle)
        )

    -- Send empties
    forM_ [1..numWorkers] $ \w -> atomically $ writeTBQueue queue Nothing

    -- Wait for all threads to be complete
    atomically $ do
        busy <- readTVar workers
        check $ busy == Map.empty

    putStrLn "Calculating result..."
    results <- readTVarIO accounts
    r <- foldM (\total (account, balanceVar) -> do
                    balance <- readTVarIO balanceVar
                    pure $ total + ((read $ unpack account) :: Integer) * balance
                ) 0 (Map.toList results)

    putStrLn $ "Result: " <> show r

    putStrLn "Done"
    pure ()

    where
        worker :: Int -> TBQueue (Maybe BS.ByteString) -> TVar (Map.Map Account (TVar Amount)) -> IO ()
        worker workerId queue accounts = do
            tx <- atomically $ readTBQueue queue
            case tx of
                Just tx' -> do
                    packet <- parseAtto transactionAtto tx'
                    atomically $ processWithTVar packet accounts
                    -- atomically $ modifyTVar' accounts $! process packet
                    worker workerId queue accounts
                Nothing -> pure ()

doNothing :: Socket -> IO ()
doNothing sock = do
    forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Client connected from: " ++ show peer
        bracket
            (socketToHandle conn ReadMode) hClose
            (\handle -> do
                hSetBuffering handle (BlockBuffering (Just 65536))
                untilM_
                    (TIO.hGetLine handle)
                    (hIsEOF handle)
            )
        putStrLn "Done"
        pure ()

main :: IO ()
main = do
    sock <- socket AF_INET Stream defaultProtocol
    let addr = SockAddrInet 7077 (tupleToHostAddress (127, 0, 0, 1))
    bind sock addr
    listen sock 1
    putStrLn "Listening on port 7077..."
    stmQueued sock
