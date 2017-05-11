{-# language OverloadedStrings #-}
{-# language TypeOperators     #-}
{-# language DeriveGeneric     #-}

module Sally.Game where 

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Aeson
import GHC.Generics
import System.Environment

-- | A user's guess
data Gs = Gs
    { likes  :: Text
    , notlikes :: Text
    } deriving (Show, Eq, Generic)

instance ToRow Gs where
    toRow (Gs lk nlk) = toRow (lk, nlk)

instance FromRow Gs where
    fromRow = Gs <$> field <*> field

instance ToJSON Gs where
    -- Filled in by DeriveGeneric
      
instance FromJSON Gs where
    -- Filled in by DeriveGeneric

-- | Sally likes guesses where she likes the first word but
-- not the second. Sally likes words with @ >= 2@ letters repeated after
-- mapping to lower case and removing spaces
isValidGs :: Gs -> Bool
isValidGs (Gs lk nlk) =
   (likes lk) && (not. likes $ nlk)
  where
    nospc = T.filter (not. isSpace)
    likes :: Text -> Bool
    likes w = 
        any (uncurry (==)) $ T.zip (nospc w) (T.tail $ nospc w)

initTable :: Connection -> IO ()
initTable conn = 
    execute_ conn
        " CREATE TABLE IF NOT EXISTS  \
        \ gs  ( likes text            \
        \     , notlikes text         \
        \     , isvalid boolean       \
        \     , time datetime default \
        \       (datetime('now','localtime')) )"

dropTable :: Connection -> IO ()
dropTable conn = 
    execute_ conn "DROP table gs"

-- | The game result of a user's guess, which records time and validity of the
-- guess. These are the objects seen by the database.
data GsRes = GsRes
    { resGs    :: Gs
    , resValid :: Bool
    , resTime  :: UTCTime
    } deriving (Generic, Show)

instance ToRow GsRes where
    toRow (GsRes gs v t) = toRow $ gs :. (Only v) :. (Only t)

instance FromRow GsRes where
    fromRow = GsRes <$> fromRow
                    <*> field
                    <*> field

instance ToJSON GsRes where

instance FromJSON GsRes where

gsResOf :: Gs -> IO (GsRes)
gsResOf gs = do
    tm <- getCurrentTime
    return $ GsRes gs (isValidGs gs) tm

-- | Grab a whole number of guesses from the database for displaying
nGuessFrom :: Int -> Connection -> IO [GsRes]
nGuessFrom n conn = query conn
    "SELECT * FROM gs order by time desc limit (?)"
    (Only n)

-- | Accept a user's guess, process it, and store the result. Time is recorded
-- by the database server.
insertGuess :: GsRes -> Connection -> IO ()
insertGuess (GsRes gs v tm) conn = do
    execute conn 
        "INSERT INTO gs (likes, notlikes, isvalid, time) values ((?),(?),(?),(?))"
        (gs :. Only v :. Only tm)
