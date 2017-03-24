{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language DeriveGeneric     #-}

module Sally.WebSockets where

import Data.Text (Text)
import qualified Data.Text as T
import Network.WebSockets as WS hiding (Message)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import Control.Monad (forever)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Network.Wai (Application, Middleware)
-- * Serializing data
import GHC.Generics
import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as J
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


-- | A websocket client
data Client = Client {
      uuid :: UUID          -- ^ Clients' UUID
    , nick :: Maybe Text    -- ^ Nickname
    , conn :: WS.Connection -- ^ Websocket connection
}

instance Eq Client where
    (==) = (==) `on` uuid

-- | Clients of the websocket app
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

broadText :: Text -> ServerState -> IO ()
broadText msg clients = do
    forM_ clients $ \(conn -> conn0) ->
        WS.sendTextData conn0 msg

data BroadcastTarget =
      AllExcept  Client
    | NoneExcept Client
    | All

data Message =
      Chat ChatMsg
    | Control ControlMsg

data ChatMsg = ChatMsg
    { chatEcho :: Bool -- ^ Are we echoing to a client their own message?
    , chatUuid :: UUID -- ^ UUID of the source
    , chatNick :: Text -- ^ Nick of the source
    , chatBody :: Text -- ^ Chat body
    } deriving (Show, Generic)

instance ToJSON ChatMsg where
instance FromJSON ChatMsg where

-- | This is somewhat dangerous becuase of the partiality of the field
-- accessors. We're only doing it here to help automate the Aeson instances
data ControlMsg = 
      YouJoined {
          newUUID :: UUID }
    | Joined {
        joinedUUID :: UUID }
    | Left {
        leftUUID :: UUID }
    deriving (Show, Generic)

sendMessage :: BroadcastTarget
            -> ServerState
            -> Message 
            -> IO ()
sendMessage tgt st msg =
    case msg of
        Chat chtmsg
            -> broadchat tgt chtmsg
  where
    broadchat tgt chtmsg =
        case tgt of
            All -> forM_ st $ \client ->
                       WS.sendTextData (conn client) (J.encode chtmsg)

wsapp :: MVar ServerState -> WS.ServerApp
wsapp state pending = do
    conn    <- WS.acceptRequest pending
    clients <- readMVar state
    newuuid <- randomIO 
    let thisClient = Client newuuid Nothing conn
        disconnect = do
            modifyMVar_ state $ \s -> do
                let s' = rmClient thisClient s
                return s'
    modifyMVar_ state $ \s -> do
        let s' = addClient thisClient s
        return s'
    flip finally disconnect $ forever $ do
        msg     <- WS.receiveData conn
        handleMsg state conn msg
    return ()

initWSState :: IO (MVar ServerState)
initWSState = newMVar emptyState

handleMsg :: MVar ServerState -> Connection -> Text -> IO ()
handleMsg st conn msg = case command of
    "Broadcast" ->
        --broadText msg =<< readMVar st
        error ""
    "Echo" -> 
        --sendText conn $ msg 
        error ""
    {-
    "Name" ->  do
        let reqnm = head payload
        nicks <- (HM.filter ((Just reqnm ==). nick)) <$> (readMVar st)
        if HM.null $ nicks
           then sendText conn $ "Username " <> reqnm <> " accepted"
           else sendText conn $ "That username is already taken!"
    "Guess" -> 
        broadcastGuess (Guess "Poodles" "Dogs" (Just "Lawrence")) st
    _ ->
        sendText conn $ "Did not understand:" <> msg
    -}
  where
    (command,rest) = T.breakOn "::" msg
    payload = T.splitOn "," $ T.drop 2 rest
