{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

module Sally.Web where

import Web.Spock hiding (text)
import Web.Spock.Config
import Web.Spock.Digestive
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Data.Text (Text)
import Data.Foldable (forM_)
import Control.Monad.IO.Class
import qualified Data.Text as T

import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, Middleware)

import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection as WC

import Control.Concurrent.MVar

import Database.SQLite.Simple

import Sally.Core
import Sally.SpockUtil
import Sally.WebSockets
import Sally.Pages
import Debug.Trace

runAppDef :: IO ()
runAppDef = runAppWith defaultConfig

runAppWith :: Configuration -> IO ()
runAppWith conf = do
    let db = dbstr conf
    withConnection db initTable
    wsSt     <- initWSState
    spockCfg <- defaultSpockCfg () PCNoDatabase  wsSt
    site     <- spockAsApp $ (spock spockCfg (spockAppWith conf))
    let app' = socketize wsSt $ serveStatic site
    run 8080 $ app'
  where
    socketize st app =
        websocketsOr WC.defaultConnectionOptions (wsapp st) app

serveStatic :: Middleware
serveStatic = staticPolicy $ hasPrefix "static"

spockAppWith :: Configuration -> SpockM () () (MVar ServerState) ()
spockAppWith conf = do
    let db = dbstr conf
    get root $ do
        (v, _)<- runForm "guess" guessForm
        gs <- liftIO $ withConnection db (selectGuesses 10)
        blaze $ mainPage v gs
    post root $ do
        (v, m) <- runForm "guess" guessForm
        case m of
            Nothing -> error "How?"
            Just gs -> do
                liftIO $ trace "here" $ withConnection db (insertGuess gs)
                st <- getState
                --liftIO $ broadcastGuess gs st
                redirect "/"
