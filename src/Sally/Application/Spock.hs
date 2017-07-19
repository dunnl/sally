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

{- We choose not to use Spock's database connection management because it would
 - have to share it the Websockets app. It is easier to manage it ourselves.
 -}

-- | Main export
makeSpockAppFrom :: AppConfig -> IO Application
makeSpockAppFrom conf = do
    spockCfg  <- defaultSpockCfg () PCNoDatabase  ()
    spockAsApp $ spock spockCfg (spockAppWith conf)

spockAppWith :: AppConfig -> SpockM () () () ()
spockAppWith (AppConfig db _ _)= do
    -- GET /
    get root $ do
        (v, _) <- runForm "guess" guessForm
        gs     <- liftIO $ withConnection db (nGuessFrom 8)
        blaze $ mainHtml v gs
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
