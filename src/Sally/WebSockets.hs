{-| Module: WebSockets
 -
 - Core functionality for the websockets-based portion of the online game
-}

{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language DeriveGeneric     #-}

module Sally.WebSockets where

import Data.Text (Text)
import qualified Data.Text as T
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.WebSockets as WS hiding (Message)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import Control.Monad (forever)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Network.Wai (Application, Middleware)
-- * Serializing data
import GHC.Generics
import           Data.Aeson (ToJSON (..), FromJSON (..), (.=), (.:))
import qualified Data.Aeson as J
-- * Database Ops
import Database.SQLite.Simple hiding (Connection)
import qualified Database.SQLite.Simple as SQL
-- * Managing client connections
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as HM
-- * Generating client IDs
import System.Random
import Data.UUID
import Data.Function(on)
import Data.Aeson.Types () -- UUID instances (requires aeson >= 1.1)
--
import Debug.Trace

import Sally.Core


-- | A websocket client. The UUID is randomly assigned to each client to
-- facilitate session management.
data Client = Client {
      uuid :: UUID          -- ^ Clients' UUID
    , conn :: WS.Connection -- ^ Websocket connection
}

-- | Clients are equal on the UUID
instance Eq Client where
    (==) = (==) `on` uuid

-- | Clients of the websocket app. This information is used to handle broadcast
-- messages.
type ServerState = HashMap UUID Client

emptyState :: ServerState
emptyState = HM.empty

numClients :: ServerState -> Int
numClients = HM.size

addClient :: Client -> ServerState -> ServerState
addClient client clients =
    if member (uuid client) clients
        then error "UUID already in clients. This shouldn't happen."
        else HM.insert (uuid client) client clients

rmClient :: Client -> ServerState -> ServerState
rmClient = HM.delete . uuid

-- | Send all connected users text
broadText :: Text -> ServerState -> IO ()
broadText msg clients = do
    forM_ clients $ \(conn -> conn0) ->
        WS.sendTextData conn0 msg

-- | When we send out messages to three types of targets:
--  * Everybody (e.g. a user's latest guess)
--  * A specific user (e.g. a welcome message)
--  * All users except one (e.g. "a new user has joined")
data BroadcastTarget =
      AllExcept  Client
    | NoneExcept Client
    | All

-- | A message from server to client
data SvMessage =
      SvGuess   GuessResult  -- ^ A new guess
    | SvControl SvControlMsg -- ^ Other messages
    deriving (Show, Generic)

data SvControlMsg = SvControlMsg Text
    deriving (Show, Generic)

instance ToJSON SvControlMsg where
instance FromJSON SvControlMsg where

instance ToJSON SvMessage where
    toJSON (SvGuess guessMsg) =
        J.object [ "type" .= "guess"
                 , "body" .= guessMsg
                 ]
    toJSON (SvControl ctlMsg) =
        J.object [ "type" .= "control"
                 , "body" .= ctlMsg
                 ]

instance FromJSON SvMessage where
    parseJSON ov@(J.Object v) = do
       flip (J.withObject "message type") ov $ \o -> do
           msg <- (o .: "message")
           case msg of
               "guess" -> MsgGuess <$> (Guess <$> (o .: "body"))

-- | Send a message out to a target group
sendMessage :: BroadcastTarget
            -> SvMessage
            -> ServerState
            -> IO ()
sendMessage tgt msg st =
    case tgt of
        All -> forM_ st $ \client ->
                   WS.sendTextData (conn client) (J.encode msg)
        AllExcept ex -> forM_ st $ \client ->
                    if client == ex
                        then return ()
                        else WS.sendTextData (conn client) (J.encode msg)
        NoneExcept ex ->
            WS.sendTextData (conn ex) (J.encode msg)

wsapp :: MVar ServerState -> WS.ServerApp
wsapp state pending = do
    conn    <- WS.acceptRequest pending
    clients <- readMVar state
    newuuid <- randomIO 
    let thisClient = Client newuuid conn
        disconnect = do
            modifyMVar_ state $ \s -> do
                let s' = rmClient thisClient s
                return s'
    modifyMVar_ state $ \s -> do
        let s' = addClient thisClient s
        return s'
    -- Start sending/recving messages
    sendMessage (NoneExcept thisClient) 
        (SvControl (SvControlMsg $ "Welcome, your UUID is " <> toText newuuid))
        =<< readMVar state
    flip finally disconnect $ forever $ do
        msg     <- WS.receiveData conn
        handleMsg state conn msg
    return ()

initWSState :: IO (MVar ServerState)
initWSState = newMVar emptyState

handleMsg :: MVar ServerState -> Connection -> ByteString -> IO ()
handleMsg st conn msg = 
    case djsonMsg of
        Nothing -> error ("Failed to handle: " ++ (show msg))
        Just jsonMsg ->
            case jsonMsg of
                CliGuess (guess -> do
                    withConnection "db/sally" (insertGuess guess)
                    sendMessage All (MsgGuess guess) =<< (readMVar st)
  where
    djsonMsg = (J.decode msg :: Maybe Message)
