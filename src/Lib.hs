{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

module Lib where

import Web.Spock hiding (text)
import Web.Spock.Config
import Web.Spock.Digestive
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Data.Foldable (forM_)
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Network.Wai.Middleware.Static

import Text.Blaze.Html (Html)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (id)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Digestive as D
import Text.Digestive.Blaze.Html5 as DB
import Control.Monad.IO.Class

import Database.SQLite.Simple


data Guess = Guess
    { likes  :: Text
    , butnot :: Text
    } deriving (Show, Eq)

instance ToRow Guess where
    toRow (Guess l n) = toRow (l, n)

instance FromRow Guess where
    fromRow = Guess <$> field <*> field

sallyLikes :: Text -> Bool
sallyLikes txt = 
    any id $ zipWith (==) (T.unpack nospaces) (T.unpack $ T.tail nospaces)
  where
    nospaces = T.filter (not. isSpace) txt

verifyGuess :: Guess -> Bool
verifyGuess (Guess likes butnot) =
   (sallyLikes likes) && (not $ sallyLikes butnot)

initTable :: Connection -> IO ()
initTable conn = 
    execute_ conn "CREATE TABLE IF NOT EXISTS guesses (likes text, butnot text, valid boolean, time datetime default (datetime('now','localtime')))"

dropTable :: Connection -> IO ()
dropTable conn = 
    execute_ conn "DROP table guesses" 

selectGuesses :: Int -> Connection -> IO [Guess :. Only Bool :. Only UTCTime]
selectGuesses n conn = query conn
    "SELECT * FROM guesses order by time desc limit (?)"
    (Only n)

insertGuess :: Guess -> Connection -> IO ()
insertGuess g conn = execute conn 
    "INSERT INTO guesses (likes, butnot, valid) values ((?),(?),(?))"
    (g :. (Only $ verifyGuess g))

askGuess :: IO (Guess)
askGuess = do
    putStr "Silly Sally likes: "
    likes <- T.pack <$> getLine
    putStr "But not: "
    butnot <- T.pack <$> getLine
    return (Guess likes butnot)

gameRound :: Connection -> IO ()
gameRound conn = do
    list <- selectGuesses 5 conn
    forM_ list $ \(g :. (Only b) :. (Only t)) -> do
        print g
        print b
    g <- askGuess
    print $ verifyGuess g
    insertGuess g conn


blaze html = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    lazyBytes. renderHtml $ html

mainLib :: IO ()
mainLib = do
    withConnection "sally" initTable
    spockCfg <- defaultSpockCfg () (PCNoDatabase)  ()
    runSpock 80 $ do
        site <- (spock spockCfg app)
        return $ serveStatic . site

serveStatic = staticPolicy $ hasPrefix "static"

bootstrap :: Html
bootstrap = do
    H.link ! rel "stylesheet" 
        ! type_ "text/css"
        ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/style.css"

app :: SpockM () () () ()
app = do
    get root $ do
        (v, _)<- runForm "guess" sallyForm
        gs <- liftIO $ withConnection "sally" (selectGuesses 10)
        blaze $ do
            H.head $ do
                H.title "Silly Sally"
                bootstrap
            H.body $ do
                sallyView v
                H.div ! class_ "guessTitle" $
                    H.h2 "Last 10 guesses"
                forM_ gs renderGuess
    post root $ do
        (v, m) <- runForm "guess" sallyForm
        case m of
            Nothing -> error "How?"
            Just g -> do
                liftIO $ withConnection "sally" (insertGuess g)
                redirect "/"

sallyForm :: (Monad m) => Form Html m Guess
sallyForm = Guess
    <$> "likes" .: D.text Nothing
    <*> "butnot" .: D.text Nothing

sallyView :: View H.Html -> H.Html
sallyView view = do
    DB.form view "/" $ do
        H.div ! class_ "line" $ do
            DB.label "likes" view "Silly Sally likes"
            H.div ! class_ "input" $ DB.inputText "likes" view
        H.div ! class_ "line" $ do
            DB.label "butnot" view "But not"
            H.div ! class_ "input" $ DB.inputText "butnot" view
        DB.inputSubmit "Submit" ! class_ "submit"

renderGuess :: (Guess :. (Only Bool) :. Only UTCTime) -> H.Html
renderGuess ((Guess l n) :. (Only b) :. (Only t)) = do
    H.div ! class_ "renderGuess" $
        p $ do
            "Silly Sally likes "
            <> renderBig l
            <> ", but not "
            <> renderBig n  <> ". "
            <> renderBool b
            <> " "
            <> renderTime t

renderBig :: Text -> H.Html
renderBig t =
    H.span ! class_ "big" $ toHtml t
renderBool :: Bool -> H.Html
renderBool True =
    H.span ! class_ "true" $ "Correct"
renderBool False =
    H.span ! class_ "false" $ "Wrong"

renderTime :: UTCTime -> H.Html
renderTime tm =
    H.span ! class_ "time" $ do
        toHtml $ formatTime defaultTimeLocale "%D %R EST" tm
