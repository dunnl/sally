{-# language OverloadedStrings #-}

module Sally.WebSockets where

import qualified Data.Text as T
import Data.Text (Text)
import Network.WebSockets as WS
import Network.WebSockets.Connection as WC
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import Control.Monad (forever)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Network.Wai.Handler.WebSockets
import Network.Wai (Application, Middleware)

import Debug.Trace

type WSClient = (Text, WS.Connection)

type ServerState = [WSClient]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

addClient :: WSClient -> ServerState -> ServerState
addClient client clients = client : clients

rmClient :: WSClient -> ServerState -> ServerState
rmClient client = filter ((/= fst client). fst)

broadcast :: Text  -> ServerState -> IO ()
broadcast msg clients = do
    --error "broadcast crashes the server!"
    forM_ clients $ trace "Called" $ \(_,conn) ->
        WS.sendTextData conn msg

wsapp :: MVar ServerState -> WS.ServerApp
wsapp state pending = do
    conn    <- WS.acceptRequest pending
    clients <- readMVar state
    modifyMVar state $ \s -> do
        let s' = addClient ("Client", conn) s
        return (s',s')
    flip finally disconnect $ forever $ do
        msg     <- WS.receiveData conn
        handleMsg state conn msg
      where
        disconnect = do
            broadcast "Someone left" =<< readMVar state
            return ()

socketize :: Application -> IO Application
socketize app = do
    st <- newMVar newServerState
    return $ websocketsOr WC.defaultConnectionOptions (wsapp st) app

handleMsg :: MVar ServerState -> Connection -> Text -> IO ()
handleMsg st conn msg = case command of
    "Broadcast" ->
        broadcast msg =<< readMVar st
        --WS.sendTextData conn $ "Broadcast " <> msg
    "Echo" -> 
        WS.sendTextData conn $ msg 
    _ ->
        WS.sendTextData conn $ "Did not understand:" <> msg

  where
    (command,rest) = T.breakOn ":" msg
    args = T.splitOn "," $ T.drop 1 rest
