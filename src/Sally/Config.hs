module Sally.Config (
      Options.Applicative.execParser
    , module Sally.Config
) where

import Control.Applicative ((<**>))
import Data.Monoid ((<>))
import System.Environment
import Options.Applicative

data AppConfig = AppConfig
    { useWebsockets :: Bool
    , dbConnStr     :: String
    , port          :: Int
    , logLevel      :: Int
    } deriving (Show, Eq)

defAppConfig :: AppConfig
defAppConfig = AppConfig
    { useWebsockets = True
    , dbConnStr     = "sally.sqlite3"
    , port          = 80
    , logLevel      = 9
    }                

data Command = Initialize | Run AppConfig
    deriving (Show)

commandParserInfo = info commandParser idm

commandParser :: Parser Command
commandParser =
    subparser
        ( (command "run" (Run <$> configParserInfo))
        <> 
          (command "initialize" (info (pure Initialize) idm))
        )

configParser :: Parser AppConfig
configParser = AppConfig
    <$> switch
        (  long "no-websockets"
        <> help "Don't use websockets"
        )
    <*> strOption
        (  long "connection"
        <> short 'c'
        <> metavar "CONNSTR"
        <> help "Sqlite3 connection string" )
    <*> option auto
        (  long "port"
        <> short 'p'
        <> help "port number"
        <> showDefault
        <> value 80
        <> metavar "PORT"
        )
    <*> option auto
        ( long "log"
        <> help "log verbosity"
        <> showDefault
        <> value 2
        <> metavar "LOG"
        )

configParserInfo :: ParserInfo AppConfig
configParserInfo =
    info (configParser <**> helper)
         (  fullDesc
         <> progDesc "Run a simple web game on PORT with a database CONNSTR"
         <> header "sally -- a simple web game"
         )
