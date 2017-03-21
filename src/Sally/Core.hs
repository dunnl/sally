{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

module Sally.Core where

import Data.Foldable (forM_)
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Data.Time.Clock (UTCTime)

data Guess = Guess
    { likes  :: Text
    , butnot :: Text
    } deriving (Show, Eq)

instance ToRow Guess where
    toRow (Guess l n) = toRow (l, n)

instance FromRow Guess where
    fromRow = Guess <$> field <*> field

sallyLikes :: Text -> Bool
sallyLikes txt = 
    any id $ zipWith (==) (T.unpack nospaces) (T.unpack $ T.tail nospaces)
  where
    nospaces = T.filter (not. isSpace) txt

verifyGuess :: Guess -> Bool
verifyGuess (Guess likes butnot) =
   (sallyLikes likes) && (not $ sallyLikes butnot)

initTable :: Connection -> IO ()
initTable conn = 
    execute_ conn
        "CREATE TABLE IF NOT EXISTS \
        \ guesses (likes text, butnot text, \
        \ valid boolean, time datetime default \
        \ (datetime('now','localtime')))"

dropTable :: Connection -> IO ()
dropTable conn = 
    execute_ conn "DROP table guesses" 

type GuessResult = Guess :. Only Bool :. Only UTCTime

selectGuesses :: Int -> Connection -> IO [Guess :. Only Bool :. Only UTCTime]
selectGuesses n conn = query conn
    "SELECT * FROM guesses order by time desc limit (?)"
    (Only n)

insertGuess :: Guess -> Connection -> IO ()
insertGuess g conn = execute conn 
    "INSERT INTO guesses (likes, butnot, valid) values ((?),(?),(?))"
    (g :. (Only $ verifyGuess g))

-- | Is this useful?
askGuess :: IO (Guess)
askGuess = do
    putStr "Silly Sally likes: "
    likes <- T.pack <$> getLine
    putStr "But not: "
    butnot <- T.pack <$> getLine
    return (Guess likes butnot)

-- | Is this useful?
gameRound :: Connection -> IO ()
gameRound conn = do
    list <- selectGuesses 5 conn
    forM_ list $ \(g :. (Only b) :. (Only t)) -> do
        print g
        print b
    g <- askGuess
    print $ verifyGuess g
    insertGuess g conn
