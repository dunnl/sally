{-# language OverloadedStrings #-}
{-# language TypeOperators     #-}
{-# language DeriveGeneric     #-}

module Sally.Core where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Data.Time.Clock (UTCTime)
import Data.Aeson
import GHC.Generics

-- | Application configuration
data Configuration = Configuration
    { dbstr :: String -- ^ Connection string passed to SQLite
    } deriving (Show, Eq)

defaultConfig = Configuration "db/sally"

-- | A user's guess
data Guess = Guess
    { likes    :: Text
    , butnot   :: Text
    } deriving (Show, Eq, Generic)

instance ToRow Guess where
    toRow (Guess l b) = toRow (l, b)

instance FromRow Guess where
    fromRow = Guess <$> field <*> field

instance ToJSON Guess where
    -- Filled in by DeriveGeneric
      
instance FromJSON Guess where
    -- Filled in by DeriveGeneric

-- | Sally likes words with two+ letters repeated after
-- mapping to lower case and removing spaces
sallyLikes :: Text -> Bool
sallyLikes txt = 
    any (uncurry (==)) $ T.zip nospaces (T.tail nospaces)
  where
    nospaces = T.filter (not. isSpace) txt

-- | Core game function. Sally likes guesses where she likes the first word but
-- not the second.
verifyGuess :: Guess -> Bool
verifyGuess (Guess likes butnot) =
   (sallyLikes likes) && (not $ sallyLikes butnot)

-- Initialize sole database table
initTable :: Connection -> IO ()
initTable conn = 
    execute_ conn
        "CREATE TABLE IF NOT EXISTS \
        \ guesses (likes text, butnot text \
        \ , valid boolean \
        \ , time datetime default \
        \ (datetime('now','localtime')) )"

-- Drop database table
dropTable :: Connection -> IO ()
dropTable conn = 
    execute_ conn "DROP table guesses" 

-- | The game result of a user's guess, which records time and validity of the
-- guess
type GuessResult = Guess :. Only Bool :. Only UTCTime

-- | Grab a whole number of guesses from the database for displaying
selectGuesses :: Int -> Connection -> IO [GuessResult]
selectGuesses n conn = query conn
    "SELECT * FROM guesses order by time desc limit (?)"
    (Only n)

-- | Accept a user's guess, process it, and store the result. Time is recorded
-- by the database server.
insertGuess :: Guess -> Connection -> IO ()
insertGuess g conn = execute conn 
    "INSERT INTO guesses (likes, butnot, valid) values ((?),(?),(?))"
    (g :. (Only $ verifyGuess g))
