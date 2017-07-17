module Sally.Config (
      Options.Applicative.execParser
    , module Sally.Config
) where

import Control.Applicative ((<**>))
import Data.Monoid ((<>))
import Options.Applicative

data AppConfig = AppConfig
    { sqliteFile :: String
    , port       :: Int
    , logLevel   :: Int
    } deriving (Show, Eq)

defAppConfig :: AppConfig
defAppConfig = AppConfig
    { sqliteFile  = "sally.sqlite3"
    , port        = 80
    , logLevel    = 9
    }                

data Command = Initialize String | Run AppConfig
    deriving (Show)

commandParserInfo = info commandParser idm

commandParser :: Parser Command
commandParser =
    subparser
        (  command "run" (Run <$> configParserInfo)
        <> command "initialize" (Initialize <$> initParserInfo) )

-- | Parser for "init" command
initParser :: Parser String
initParser = 
    strOption ( long "database"
              <> metavar "DATAFILE"
              <> help "Sqlite3 database file to create"
              )
initParserInfo :: ParserInfo String
initParserInfo = 
    info (initParser <**> helper)
             (  fullDesc
             <> progDesc "Create a sqlite3 database to store user submissions"
             )

configParser :: Parser AppConfig
configParser = AppConfig
    <$> strOption
        (  long "database"
        <> short 'd'
        <> metavar "DATAFILE"
        <> help "Sqlite3 database file" )
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
         <> progDesc "Run a simple web game on port PORT using the Sqlite3 database DATAFILE"
         <> header "sally -- a simple web game"
         )
