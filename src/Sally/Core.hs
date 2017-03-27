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

-- | A guess submitted over the app
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

sallyLikes :: Text -> Bool
sallyLikes txt = 
    any (uncurry (==)) $ T.zip nospaces (T.tail nospaces)
  where
    nospaces = T.filter (not. isSpace) txt

verifyGuess :: Guess -> Bool
verifyGuess (Guess likes butnot) =
   (sallyLikes likes) && (not $ sallyLikes butnot)

initTable :: Connection -> IO ()
initTable conn = 
    execute_ conn
        "CREATE TABLE IF NOT EXISTS \
        \ guesses (likes text, butnot text \
        \ , valid boolean \
        \ , time datetime default \
        \ (datetime('now','localtime')) )"

dropTable :: Connection -> IO ()
dropTable conn = 
    execute_ conn "DROP table guesses" 

type GuessResult = Guess :. Only Bool :. Only UTCTime

selectGuesses :: Int -> Connection -> IO [GuessResult]
selectGuesses n conn = query conn
    "SELECT * FROM guesses order by time desc limit (?)"
    (Only n)

insertGuess :: Guess -> Connection -> IO ()
insertGuess g conn = execute conn 
    "INSERT INTO guesses (likes, butnot, valid) values ((?),(?),(?))"
    (g :. (Only $ verifyGuess g))
