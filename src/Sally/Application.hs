{-|
   Module: Sally.Application
   Description: Central application module
   Maintainer: lawrence.dunn.iii@gmail.com
   License: MIT
-}

module Sally.Application where

import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (staticPolicy, hasPrefix)

import Sally.Config
import Sally.Application.WebSockets
import Sally.Application.Spock

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
    sp <- makeSpockAppFrom conf
    let sp' = withStatic sp
    fullApp <- addSocketsApp conf sp'
    run (port conf) fullApp
