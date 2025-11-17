{-# LANGUAGE OverloadedStrings #-}

module ParserMega where
import           Control.Monad        (void)
import           Data.Char            (digitToInt)
import           Data.Text            (Text, pack)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Types

-- Most practically useful Parser in any context
type ParserMega m a = ParsecT Void Text m a

transactionMega :: Monad m => ParserMega m Transaction
transactionMega = do
    !seq' :: Integer <- string "Transaction (seq: " *> amount <* string ") => "
    !action <- choice
                [ string "OpenAccount " *> (OpenAccount <$> account)
                , string "Deposit " *> (Deposit <$> (amount <* string " to ") <*> account)
                , string "Transfer " *> (Transfer <$> amount <*> (string " from " *> account) <*> (string " to " *> account))
                ]
    void $ optional newline
    pure $ Transaction seq' action
    where
        account :: ParserMega m Text
        account = (\s -> let !n = pack s in n) <$> some digitChar
        -- Keep for historical reasons - using read was very space leaky
        --amount = (\s -> let !n = read s in n) <$> some digitChar
        amount :: ParserMega m Integer
        amount = fromIntegral <$> foldl' (\acc c -> acc * 10 + digitToInt c) 0 <$> some digitChar

parseMega :: forall m a. Monad m => ParserMega m a -> Text -> m a
parseMega parser input = do
    result <- runParserT parser "(input)" input
    case result of
        Left err -> error $ "\nA terrible parsing error occured:\n" <> errorBundlePretty err
        Right a  -> return a
