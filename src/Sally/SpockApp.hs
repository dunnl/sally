{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

module Sally.SpockApp where

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

import Sally.Game
import Sally.SpockUtil
import Sally.SocketApp
import Sally.Pages
import Debug.Trace

runAppDef :: IO ()
runAppDef = runAppWith defAppConf

runAppWith :: AppConf-> IO ()
runAppWith conf = do
    let db = connStr conf
    withConnection db initTable
    wsSt     <- initState
    spockCfg <- defaultSpockCfg () PCNoDatabase  wsSt
    site     <- spockAsApp $ (spock spockCfg (spockAppWith conf))
    let app' = socketize wsSt $ serveStatic site
    run 8080 $ app'
  where
    socketize st app =
        websocketsOr WC.defaultConnectionOptions (wsapp st) app

serveStatic :: Middleware
serveStatic = staticPolicy $ hasPrefix "static"

spockAppWith :: AppConf -> SpockM () () (MVar ServerState) ()
spockAppWith conf = do
    let db = connStr conf

    get root $ do
        (v, _) <- runForm "guess" guessForm
        gs     <- liftIO $ withConnection db (nGuessFrom 8)
        blaze $ mainHtml v gs

    post root $ do
        (_, m) <- runForm "guess" guessForm
        case m of
            Nothing ->
                -- This shouldn't happen because there is no validation
                error "guess form failed validation"
            Just gs -> do
                liftIO $ do
                    res <- gsResOf gs
                    withConnection db (insertGuess res)
                st <- getState
                --liftIO $ broadcastGuess gs st
                redirect "/"
