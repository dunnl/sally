{-| Module: WebSockets
 -
 - Core functionality for the websockets-based portion of the online game
-}

{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language DeriveGeneric     #-}

module Sally.SocketApp where

import Control.Monad (forever)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), (.:))
import qualified Data.Aeson as J
import Data.Aeson.Types as J (Parser)-- And UUID instances (requires aeson >= 1.1)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (forM_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.UUID
import Data.Function (on)
import qualified Data.Text as T
import Database.SQLite.Simple hiding (Connection)
import GHC.Generics
import Network.WebSockets as WS hiding (Message)
import Network.WebSockets.Connection as WC
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai (Application)
import System.Random

import Sally.Game
import Sally.Config

-- | A websocket client. 
data Client = Client {
      uuid :: UUID          -- ^ Clients' UUID
    , conn :: WS.Connection -- ^ Websocket connection
}

instance Show Client where
    show (Client uuid _) = "Client " ++ show uuid

instance Eq Client where
    (==) = (==) `on` uuid

type ServerState = HashMap UUID Client

emptyState :: ServerState
emptyState = HM.empty

newServerState :: IO (MVar ServerState)
newServerState = newMVar emptyState

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

sendMessage :: BroadcastTarget
            -> SvMessage
            -> ServerState
            -> IO ()
sendMessage tgt msg st =
    case tgt of
        All ->
            forM_ st $ \client -> do
                WS.sendTextData (conn client) (J.encode msg)
        AllExcept ex -> 
            forM_ st $ \client ->
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

addClientTo :: Client -> MVar ServerState -> IO ()
addClientTo cl mst = do
    modifyMVar_ mst $ \st -> do
        return $ addClient cl st
        
clientLeaves :: Client -> MVar ServerState -> IO ()
clientLeaves cl@(Client uuid _) mst = do
    modifyMVar_ mst $ \st -> do
        sendMessage (AllExcept cl) byeMsg st
        broadcastNumClients st
        return $ rmClient cl st
  where
    byeMsg = 
        SvCtrl $ SvCtrlMsg $ "A client left: " <> toText uuid
    
greetClient :: Client -> MVar ServerState -> IO ()
greetClient cl@(Client uuid _) mst = do
    withMVar mst $ \st -> do
        sendMessage (NoneExcept cl) greetMsg st
        sendMessage (AllExcept cl) annMsg    st
        broadcastNumClients st
  where
    greetMsg = 
        SvCtrl (SvCtrlMsg $ "Welcome, your UUID is " <> toText uuid)
    annMsg =
        SvCtrl (SvCtrlMsg $ "A new client joined with UUID " <> toText uuid)

webSocketsApp :: AppConfig -> MVar ServerState -> WS.ServerApp
webSocketsApp conf state pending = do
    conn    <- WS.acceptRequest pending
    uuid <- randomIO 
    WS.forkPingThread conn 30
    let thisClient = Client uuid conn
    addClientTo thisClient state
    greetClient thisClient state
    flip finally (clientLeaves thisClient state) $ forever $ do
        msg     <- WS.receiveData conn
        handleClientMsg conf state conn msg

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
            withConnection (sqliteFile conf) (insertGuess res)
            sendMessage All (SvGs res) =<< (readMVar st)
  where
    msg = (J.decode encmsg :: Maybe ClMessage)


withSockets :: AppConfig -> Application -> IO Application
withSockets conf app = do
    wsSt <- newServerState 
    return $ websocketsOr WC.defaultConnectionOptions (webSocketsApp conf wsSt) app
