{-|
   Module: Sally.Application.Spock
   Description: Spock component of application
   Maintainer: lawrence.dunn.iii@gmail.com
   License: MIT
-}

{-# language OverloadedStrings #-}

module Sally.Application.Spock (
    makeSpockAppFrom
) where

import Control.Monad.IO.Class (liftIO)
import Data.Text ()
import Database.SQLite.Simple
import Network.Wai (Application, Middleware)
import Network.Wai.Middleware.Static (staticPolicy, hasPrefix)
import Web.Spock hiding (text)
import Web.Spock.Config
import Web.Spock.Digestive (runForm)

import Sally.Game
import Sally.Config
import Sally.SpockUtil
import Sally.Pages
import Sally.Application.Websockets

{- We choose not to use Spock's database connection management because it would
 - have to share it the Websockets app. It is easier to manage it ourselves.
 -}

type NoSession = ()
type NoDatabase = ()
type SpockState = ServerState

-- | Main export
makeSpockAppFrom :: AppConfig -> SpockState -> IO Application
makeSpockAppFrom conf state = do
    spockCfg  <- defaultSpockCfg NoSession PCNoDatabase state
    spockAsApp $ spock spockCfg (spockAppWith conf)

MySpockM = SpockM NoSession NoDatabase SpockState

spockAppWith :: AppConfig -> MySpockM ()
spockAppWith (AppConfig db _ _)= do
    -- GET /
    get root $ do
        (v, _) <- runForm "guess" guessForm
        --gs     <- liftIO $ withConnection db (nGuessFrom 8)
        blaze $ mainHtml v []
    get "about" $ do
        blaze aboutHtml
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
