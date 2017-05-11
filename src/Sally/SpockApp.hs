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
import Sally.Config
import Sally.SpockUtil
import Sally.SocketApp
import Sally.Pages
import Debug.Trace

runAppDispatch :: IO ()
runAppDispatch = 
    do cmd <- execParser $ commandParserInfo
       print cmd

runWith :: AppConfig -> IO ()
runWith conf = do
    let db = dbConnStr conf

    wsSt      <- initWebsockets
    spockCfg  <- defaultSpockCfg () PCNoDatabase  wsSt
    spockApp  <- spockAsApp $ (spock spockCfg (spockAppWith conf))

    let spockApp' = fmap withStatic spockApp
    app       <- if   useWebsockets conf
                 then withSockets spockApp'
                 else spockApp'
    run (port conf) $ app
  where
    withSockets app = do
        wsSt      <- initWebsockets
        websocketsOr WC.defaultConnectionOptions (wsapp conf st) app

withStatic :: Middleware
withStatic = staticPolicy $ hasPrefix "static"

spockAppWith :: AppConfig -> SpockM () () (MVar ServerState) ()
spockAppWith conf = do
    let db = dbConnStr conf

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
