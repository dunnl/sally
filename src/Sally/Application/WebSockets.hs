{-|
   Module: Sally.Application.WebSockets
   Description: Websockets component of application
   Maintainer: lawrence.dunn.iii@gmail.com
   License: MIT
-}

{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language DeriveGeneric     #-}

-- Docker test
module Sally.Application.WebSockets where

import Control.Monad (forever, when)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), (.:))
import qualified Data.Aeson as J
import Data.Aeson.Types as J (Parser)-- And UUID instances (requires aeson >= 1.1)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (forM_)
import Data.Map.Strict (Map)
import Data.Map.Strict as Map
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

-- | A client may choose to subscribe either to their own guesses, or to all
-- guesses submitted by all users
data Subscription = SubAll | SubSelf
    deriving (Show, Eq, Generic)

instance FromJSON Subscription

-- | A websocket client. 
data Client = Client {
      clUuid :: UUID          -- ^ Clients' UUID
    , clConn :: WS.Connection -- ^ Websocket connection
    , clSub  :: Subscription  -- ^ The client's subscription
}

instance FromJSON ClientGuess
    -- Filled in by DeriveGeneric

instance Show Client where
    show (Client uuid _ _) = "Client " ++ show uuid

instance Eq Client where
    (==) = (==) `on` clUuid

type ServerState = Map UUID Client

emptyState :: ServerState
emptyState = Map.empty

newServerState :: IO (MVar ServerState)
newServerState = newMVar emptyState

numClients :: ServerState -> Int
numClients = Map.size

addClient :: Client -> ServerState -> ServerState
addClient client clients =
    if member (clUuid client) clients
        then error "UUID already in clients. This shouldn't happen."
        else Map.insert (clUuid client) client clients

rmClient :: Client -> ServerState -> ServerState
rmClient = Map.delete . clUuid

resetState :: ServerState -> ServerState
resetState = const emptyState

subscribeClient :: Client -> Subscription -> MVar ServerState -> IO ()
subscribeClient cl sub st =
    modifyMVar_ st $
        return. Map.adjust (\cl0 -> cl0 {clSub = sub}) (clUuid cl)

-- | A message from server to client
data SvMessage =
      SvGs   GsRes     -- ^ A new guess
    | SvUuid Text      -- ^ Set user's UUID
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
    toJSON (SvUuid uuid) =
        J.object [ "type" .= ("uuid" :: Text)
                 , "body" .= uuid
                 ]
    toJSON (SvCtrl ctlMsg) =
        J.object [ "type" .= ("control" :: Text)
                 , "body" .= ctlMsg
                 ]

-- | A client's submitted guess
data ClientGuess = ClientGuess
    { clLikes :: Text
    , clNotLikes :: Text
    } deriving (Show, Eq, Generic)

clientToGuess :: UUID -> ClientGuess -> Gs
clientToGuess uuid (ClientGuess lk nlk) =
    Gs lk nlk (toText uuid)

data ClMessage =
      ClMsgGs ClientGuess
    | ClMsgRenew Subscription

instance FromJSON ClMessage where
    parseJSON = do
       J.withObject "Client message" $ \obj-> do
           msg <- (obj .: "type") :: J.Parser Text
           case msg of
               "guess" -> ClMsgGs <$> (obj .: "body")
               "renew" -> ClMsgRenew <$> (obj .: "body")
               _ -> fail "Did not understand client message type"

broadcast :: (Client -> Bool)
          -> SvMessage
          -> ServerState
          -> IO ()
broadcast pred msg st =
    forM_ st $ \client -> do
        when (pred client) $
            WS.sendTextData (clConn client) (J.encode msg)

allClients :: Client -> Bool
allClients = const True

allSubscribed :: Client -> Bool
allSubscribed c = SubAll == clSub c

toClient :: Client -> Client -> Bool
toClient client = (==) client

broadcastNumClients :: ServerState -> IO ()
broadcastNumClients st =
    broadcast allClients
        (SvCtrl (SvCtrlMsg $ "Active (socket-based) clients: " <> (T.pack. show $ numClients st))) st

addClientTo :: Client -> MVar ServerState -> IO ()
addClientTo cl mst = do
    modifyMVar_ mst $ \st -> do
        return $ addClient cl st
    print "A client has entered. New state:"
    debugState mst

debugState :: MVar ServerState -> IO ()
debugState mst = do
    st <- readMVar mst
    print st
        
clientLeaves :: Client -> MVar ServerState -> IO ()
clientLeaves cl mst = do
    modifyMVar_ mst $ return. rmClient cl
    broadcast (not . toClient cl) byeMsg =<< mst
    broadcastNumClients st =<< mst
    print "A client is leaving. New state:"
    debugState mst
  where
    byeMsg = 
        SvCtrl $ SvCtrlMsg $ "A client left: " <> toText (clUuid cl)
    
greetClient :: Client -> MVar ServerState -> IO ()
greetClient cl mst = do
    withMVar mst $ \st -> do
        -- Say hi to the new client
        broadcast (toClient cl) greetMsg st
        -- Send the new client their UUID
        broadcast (toClient cl) (SvUuid $ toText thisUuid) st
        -- Let others know of a new arrival
        broadcast (not . toClient cl) annMsg    st
        -- Remind everybody how many clients are connected
        broadcastNumClients st
  where
    thisUuid = clUuid cl
    greetMsg = 
        SvCtrl (SvCtrlMsg $ "Welcome, your UUID is " <> toText thisUuid)
    annMsg =
        SvCtrl (SvCtrlMsg $ "A new client joined with UUID " <> toText thisUuid)

webSocketsApp :: AppConfig -> MVar ServerState -> WS.ServerApp
webSocketsApp conf state pending = do
    conn    <- WS.acceptRequest pending
    thisUuid <- randomIO 
    WS.forkPingThread conn 30
    let thisClient = Client thisUuid conn SubSelf
    addClientTo thisClient state
    greetClient thisClient state
    flip finally (clientLeaves thisClient state) $ forever $ do
        msg     <- WS.receiveData conn
        handleClientMsg conf state thisClient msg

handleClientMsg :: AppConfig
                -> MVar ServerState
                -> Client
                -> ByteString
                -> IO ()
handleClientMsg conf st client encmsg = 
    case msg of
        Nothing ->
            error ("Failed to handle: " ++ (show encmsg))
        Just (ClMsgGs guess) -> do
            res <- gsResOf (clientToGuess thisUuid guess)
            withConnection (sqliteFile conf) (insertGuess res)
            case clSub client of
                SubSelf ->
                    broadcast (\c -> (toClient client c || allSubscribed c)) (SvGs res) =<< (readMVar st)
                SubAll ->
                    broadcast allSubscribed (SvGs res) =<< (readMVar st)
        Just (ClMsgRenew subc) -> do
            let getGuesses = if subc == SubAll
                             then  nGuessFrom 8
                             else  nGuessFromUser 8 thisUuid
            guesses <- withConnection (sqliteFile conf) getGuesses
            subscribeClient client subc st
            forM_ (reverse guesses) $ \guess ->
                broadcast (toClient client) (SvGs guess) =<< readMVar st
  where
    msg = (J.decode encmsg :: Maybe ClMessage)
    thisUuid = clUuid client

-- | The main exported function, which accepts global application configuration
-- data and wraps a WAI.Application with this websocket app
wrapWithSockets :: AppConfig -> MVar ServerState -> Application -> IO Application
wrapWithSockets conf wsSt app = do
    return $ websocketsOr WC.defaultConnectionOptions (webSocketsApp conf wsSt) app
