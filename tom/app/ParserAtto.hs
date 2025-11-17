{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserAtto where
import           Control.Applicative
import           Control.Monad        (void)
import           Data.Attoparsec.Text
import           Data.Char            (digitToInt, isDigit)
import           Data.Text            (Text, pack)
import           Types

type ParserAtto a = Parser a

transactionAtto :: ParserAtto Transaction
transactionAtto = do
    !seq' :: Int <- string "Transaction (seq: " *> amount <* string ") => "
    !action <- choice
                [ string "OpenAccount " *> (OpenAccount <$> account)
                , string "Deposit " *> (Deposit <$> (amount <* string " to ") <*> account)
                , string "Transfer " *> (Transfer <$> amount <*> (string " from " *> account) <*> (string " to " *> account))
                ]
    void $ optional endOfLine
    pure $ Transaction seq' action
    where
        account :: ParserAtto Text
        account = takeWhile1 isDigit
        amount = decimal

parseAtto :: Monad m => ParserAtto a -> Text -> m a
parseAtto parser input =
    case parseOnly parser input of
        Left err -> error $ "\nA terrible parsing error occured:\n" <> err
        Right a  -> pure a
