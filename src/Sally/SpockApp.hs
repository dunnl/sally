{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

module Sally.SpockApp where

import Control.Monad.IO.Class (liftIO)
import Data.Text ()
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (staticPolicy, hasPrefix)
import Web.Spock hiding (text)
import Web.Spock.Config
import Web.Spock.Digestive (runForm)

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
       case cmd of
            Initialize file -> initialize
            Run cfg         -> runMainWith cfg

initialize :: IO ()
initialize = print "Not implemented"

withStatic :: Middleware
withStatic = staticPolicy $ hasPrefix "static"

runMainWith :: AppConfig -> IO ()
runMainWith conf = do
    spockCfg  <- defaultSpockCfg () PCNoDatabase  ()
    baseApp  <- spockAsApp $ (spock spockCfg (spockAppWith conf)) :: IO Application
    let waiApp = withStatic baseApp
    fullApp <- withSockets conf waiApp
    run (port conf) fullApp

spockAppWith :: AppConfig -> SpockM () () () ()
spockAppWith (AppConfig db _ _)= do
    -- GET /
    get root $ do
        (v, _) <- runForm "guess" guessForm
        gs     <- liftIO $ withConnection db (nGuessFrom 8)
        blaze $ mainHtml v gs
    -- POST /
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
