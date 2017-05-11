{-| Module: WebSockets
 -
 - Core functionality for the websockets-based portion of the online game
-}

{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language DeriveGeneric     #-}

module Sally.SocketApp where

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
import           Data.Aeson ( ToJSON (..)
                            , FromJSON (..)
                            , (.=)
                            , (.:))
import qualified Data.Aeson as J
-- * Database Ops
import Database.SQLite.Simple hiding (Connection)
import qualified Database.SQLite.Simple as SQL
import Data.Aeson.Types as J (Parser)-- And UUID instances (requires aeson >= 1.1)
-- * Managing client connections
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as HM
-- * Generating client IDs
import System.Random
import Data.UUID
import Data.Function(on)
--
import Debug.Trace

import Sally.Game
import Sally.Config


-- | A websocket client. 
data Client = Client {
      uuid :: UUID          -- ^ Clients' UUID
    , conn :: WS.Connection -- ^ Websocket connection
}

instance Show Client where
    show (Client uuid _) = show uuid

instance Eq Client where
    (==) = (==) `on` uuid

type ServerState = HashMap UUID Client

emptyState :: ServerState
emptyState = HM.empty

initWebsockets :: IO (MVar ServerState)
initWebsockets = newMVar emptyState

numClients :: ServerState -> Int
numClients = HM.size

addClient :: Client -> ServerState -> ServerState
addClient client clients =
    if member (uuid client) clients
        then error "UUID already in clients. This shouldn't happen."
        else HM.insert (uuid client) client clients

rmClient :: Client -> ServerState -> ServerState
rmClient = HM.delete . uuid

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
      SvGs   GsRes     -- ^ A new guess
    | SvCtrl SvCtrlMsg -- ^ Other messages
    deriving (Show, Generic)

data SvCtrlMsg = SvCtrlMsg Text
    deriving (Show, Generic)

instance ToJSON SvCtrlMsg where

instance ToJSON SvMessage where
    toJSON (SvGs gres) =
        J.object [ "type" .= ("guess" :: Text)
                 , "body" .= gres
                 ]
    toJSON (SvCtrl ctlMsg) =
        J.object [ "type" .= ("control" :: Text)
                 , "body" .= ctlMsg
                 ]

data ClMessage =
      ClGs Gs

instance FromJSON ClMessage where
    parseJSON ov@(J.Object v) = do
       flip (J.withObject "message type") ov $ \o -> do
           msg <- (o .: "type") :: J.Parser Text
           case msg of
               "guess" -> ClGs <$> (o .: "body")

-- | Send a message out to a target group. We don't generalize SvMessage to
-- (ToJSON a)=>a to ensure message type-safety for the client
sendMessage :: BroadcastTarget
            -> SvMessage
            -> ServerState
            -> IO ()
sendMessage tgt msg st =
    case tgt of
        All -> forM_ st $ \client -> do
                    trace  ("Sending message to " ++ show client)
                     WS.sendTextData (conn client) (J.encode msg)
        AllExcept ex -> forM_ st $ \client ->
                    if client == ex
                        then return ()
                        else WS.sendTextData (conn client) (J.encode msg)
        NoneExcept ex ->
            WS.sendTextData (conn ex) (J.encode msg)

broadcastNumClients :: ServerState -> IO ()
broadcastNumClients st =
    sendMessage All
        (SvCtrl (SvCtrlMsg $ "Active (socket-based) clients: " <> (T.pack. show $ numClients st)))
        st
        
wsapp :: AppConfig -> MVar ServerState -> WS.ServerApp
wsapp conf state pending = do
    conn    <- WS.acceptRequest pending
    clients <- readMVar state
    newuuid <- randomIO 
    WS.forkPingThread conn 30
    let thisClient = Client newuuid conn
        disconnect = do
            modifyMVar_ state $ \s -> do
                let s' = rmClient thisClient s
                return s'
            sendMessage (AllExcept thisClient) 
                (SvCtrl (SvCtrlMsg $ "A client left: " <> toText newuuid))
                =<< readMVar state
            broadcastNumClients =<< readMVar state
    modifyMVar_ state $ \s -> do
        let s' = addClient thisClient s
        return s'
    -- Start sending/recving messages
    sendMessage (NoneExcept thisClient) 
        (SvCtrl (SvCtrlMsg $ "Welcome, your UUID is " <> toText newuuid))
        =<< readMVar state
    sendMessage (AllExcept thisClient) 
        (SvCtrl (SvCtrlMsg $ "A new client joined with UUID " <> toText newuuid))
        =<< readMVar state
    broadcastNumClients =<< readMVar state
    flip finally disconnect $ forever $ do
        msg     <- WS.receiveData conn
        handleClientMsg conf state conn msg
    return ()

handleClientMsg :: AppConfig
                -> MVar ServerState
                -> Connection
                -> ByteString
                -> IO ()
handleClientMsg conf st conn encmsg = 
    case msg of
        Nothing ->
            error ("Failed to handle: " ++ (show encmsg))
        Just (ClGs guess) -> do
            res <- gsResOf guess
            withConnection (dbConnStr conf) (insertGuess res)
            sendMessage All (SvGs res) =<< (readMVar st)
  where
    msg = (J.decode encmsg :: Maybe ClMessage)
