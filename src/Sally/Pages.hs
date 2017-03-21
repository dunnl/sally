{-# language OverloadedStrings #-}
{-# language TypeOperators #-}


module Sally.Pages where

import Text.Blaze.Html (Html)
import Data.Monoid ((<>))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (id)
import qualified Text.Blaze.Html5.Attributes as A (id)
import Text.Digestive as D
import Text.Digestive.Blaze.Html5 as DB
import Database.SQLite.Simple
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Data.Text (Text)
import Data.Foldable (forM_)

import Sally.Core

bootstrap :: Html
bootstrap = do
    H.link ! rel "stylesheet" 
        ! type_ "text/css"
        ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/style.css"
    H.script ! src "/static/app.js" $ ""

mainPage :: View H.Html -> [GuessResult] -> Html
mainPage v gs = do
    H.head $ do
        H.title "Silly Sally"
        bootstrap
    H.body $ do
        H.div ! A.class_ "container" $ do
            H.div ! A.class_ "row" $ do
                socketsDiv
                H.div ! A.class_ "col-md-6" $ do
                    H.header "Silly sally"
                    sallyView v
                    H.div ! class_ "guessTitle" $
                        H.h2 "Last 10 guesses"
                    forM_ gs renderGuess
socketsDiv :: Html 
socketsDiv = do
    H.div ! A.class_ "col-md-6" $ do
        H.header "Websockets"
        H.div $ H.form $ do
            H.label ! A.for "username.input" $ "Username: "
            H.input ! A.id   "username.input"
                    ! A.name "username.input"
        H.hr
        H.div $ H.form ! A.id "chat.form" $ do
            H.label ! A.for "chat.input" $ "Broadcast: "
            H.input ! A.id   "chat.input"
                    ! A.name "chat.input"
        H.hr
        H.div ! A.id "websocket-div" $ do
            H.ul ! A.id "websocket-ul" $ ""

sallyForm :: (Monad m) => Form Html m Guess
sallyForm = Guess
    <$> "likes" .: D.text Nothing
    <*> "butnot" .: D.text Nothing

sallyView :: View H.Html -> H.Html
sallyView view = do
    DB.form view "/" ! A.id "guess.form" $ do
        H.div ! class_ "line" $ do
            DB.label "likes" view "Silly Sally likes"
            H.div ! class_ "input" $ DB.inputText "likes" view
        H.div ! class_ "line" $ do
            DB.label "butnot" view "But not"
            H.div ! class_ "input" $ DB.inputText "butnot" view
        DB.inputSubmit "Submit" ! class_ "submit"

renderGuess :: GuessResult -> H.Html
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
