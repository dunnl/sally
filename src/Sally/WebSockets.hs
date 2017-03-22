{-# language OverloadedStrings #-}

module Sally.WebSockets where

import qualified Data.Text as T
import Data.Text (Text)
import Network.WebSockets as WS
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import Control.Monad (forever)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Network.Wai (Application, Middleware)
import qualified Data.Aeson as J

import Debug.Trace

import Sally.Core

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
    forM_ clients $ trace "Called" $ \(_,conn) ->
        WS.sendTextData conn msg

broadcastGuess :: Guess  -> MVar ServerState -> IO ()
broadcastGuess guess mst = withMVar mst $ \clients -> do
    forM_ clients $ \(_,conn) ->
        WS.sendTextData conn $ "Guess:: " <> J.encode guess

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

initWSState :: IO (MVar ServerState)
initWSState = newMVar newServerState

handleMsg :: MVar ServerState -> Connection -> Text -> IO ()
handleMsg st conn msg = case command of
    "Broadcast" ->
        broadcast msg =<< readMVar st
        --WS.sendTextData conn $ "Broadcast " <> msg
    "Echo" -> 
        WS.sendTextData conn $ msg 
    "Guess" -> 
        broadcastGuess (Guess "Poodles" "Dogs" (Just "Lawrence")) st
    _ ->
        WS.sendTextData conn $ "Did not understand:" <> msg

  where
    (command,rest) = T.breakOn "::" msg
    args = T.splitOn "," $ T.drop 1 rest
