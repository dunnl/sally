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

-- | Entry point into the application. We dispatch what to do next based on the
-- COMMAND pased by the user
runAppDispatch :: IO ()
runAppDispatch = 
    do cmd <- execParser $ commandParserInfo
       case cmd of
            Initialize file -> initialize
            Run cfg         -> runMainWith cfg

initialize :: IO ()
initialize = print "Not implemented"

-- | Augment a WAI app by serving a static directory
withStatic :: Middleware
withStatic = staticPolicy $ hasPrefix "static"

-- | Run the main application server
runMainWith :: AppConfig -> IO ()
runMainWith conf = do
    sp <- makeSpockAppFrom conf
    let sp' = withStatic sp
    fullApp <- addSocketsApp conf sp'
    print "Running sally"
    run (port conf) fullApp
