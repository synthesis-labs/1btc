module Types where
import           Data.Text (Text)

data Action = OpenAccount !Account | Deposit !Amount !Account | Transfer !Amount !Account !Account deriving (Show)
data Transaction = Transaction !Integer !Action deriving (Show)
type Account = Text
type Amount = Integer
