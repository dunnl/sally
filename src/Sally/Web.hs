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

import Database.SQLite.Simple

import Sally.Core
import Sally.SpockUtil
import Sally.WebSockets
import Sally.Pages

mainLib :: IO ()
mainLib = do
    withConnection "db/sally" initTable
    spockCfg <- defaultSpockCfg () (PCNoDatabase)  ()
    site <- spockAsApp $ (spock spockCfg app)
    app' <- socketize $ (serveStatic site :: Application)
    run 8080 $ app'

serveStatic = staticPolicy $ hasPrefix "static"

app :: SpockM () () () ()
app = do
    get root $ do
        (v, _)<- runForm "guess" sallyForm
        gs <- liftIO $ withConnection "db/sally" (selectGuesses 10)
        blaze $ mainPage v gs
    post root $ do
        (v, m) <- runForm "guess" sallyForm
        case m of
            Nothing -> error "How?"
            Just g -> do
                liftIO $ withConnection "db/sally" (insertGuess g)
                redirect "/"
