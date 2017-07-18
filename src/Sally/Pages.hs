{-|
   Module: Sally.Pages
   Description: Blaze Html data
   Maintainer: lawrence.dunn.iii@gmail.com
   License: MIT
-}

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

import Sally.Game

includes :: Html
includes = do
    H.link   ! rel "stylesheet" 
             ! type_ "text/css"
             ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    H.link   ! rel "stylesheet" ! type_ "text/css" ! href "/static/style.css"
    H.script ! src "/static/app.js" $ ""
    H.script ! src "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.18.1/moment.min.js" $ ""

mainHtml :: View H.Html -> [GsRes] -> Html
mainHtml v gsrs = do
    H.head $ do
        H.title "Silly Sally"
        includes
    H.body $ do
        H.div ! A.class_ "container" $ do
            navbar
            welcome
            faq
            H.div ! A.class_ "row" $ do
                H.div ! A.class_ "col-md-6" $ do
                    socketsDiv
                H.div ! A.class_ "col-md-6" $ do
                    gameDiv v gsrs

welcome :: Html
welcome = do
    H.div ! A.class_ "row" $ do
        H.div ! A.class_ "col-md-8 col-md-offset-2" $ do
            H.header $
                H.h1 $ do
                    "Silly Sally"
                    H.small ! A.class_ "small-header" $ 
                        "Provides minutes of entertainment"

faq :: Html
faq = do
    H.div ! A.class_ "row" $ do
        H.section ! A.class_ "col-md-8 col-md-offset-2 qa-header" $ do
            H.dl ! A.id "qa-list" $ do
                H.dt $ do
                    H.span ! A.class_ "qa-lead" $ "Q:"
                    H.div ! A.class_ "qa-content" $
                        "What is this?"
                H.dd $ do
                    H.span ! A.class_ "qa-lead" $ "A:"
                    H.div ! A.class_ "qa-content" $ do
                        "Silly Sally is a game played on road trips and in waiting rooms. \
                        \ The idea is simple: figure out what Silly Sally likes. \
                        \ Here's an example to get you started: "
                        H.span ! A.class_ "example" $
                            "Silly Sally likes Haskell, but not Python."
navbar :: Html
navbar = do
    H.nav ! A.class_ "navbar" $ do
        H.div ! A.class_ "container" $ do
            H.div ! A.class_ "navbar-header" $
                H.a ! A.href "/" ! A.class_ "navbar-brand" $
                    "Home"
            H.ul ! A.class_ "nav navbar-nav navbar-right" $ do
                H.li $ H.a ! href "about" $ "About"
                H.li $ H.a ! href "data" $ "Data"

socketsDiv :: Html 
socketsDiv = do
    H.header "Program messages"
    H.hr
    H.div ! A.id "message-div" $ do
        H.ul ! A.id "message-ul" $ ""

gameDiv :: View H.Html -> [GsRes] -> Html
gameDiv v gsrs = do
    H.header "Submit a guess"
    H.hr
    guessView v
    H.div ! class_ "guessTitle" $
        H.h2 "Last 8 guesses"
    H.ul ! A.id "guess-ul" $ do
        forM_ gsrs prettyGuess

guessForm :: (Monad m) => Form Html m Gs
guessForm = Gs
    <$> "likes"    .: D.text Nothing
    <*> "notlikes" .: D.text Nothing

guessView :: View H.Html -> H.Html
guessView view = do
    DB.form view "/" ! A.id "guess-form" $ do
        H.div ! class_ "line" $ do
            DB.label "likes" view "Silly Sally likes"
            H.div ! class_ "input" $ DB.inputText "likes" view
        H.div ! class_ "line" $ do
            DB.label "notlikes" view "But not"
            H.div ! class_ "input" $ DB.inputText "notlikes" view
        DB.inputSubmit "Submit" ! class_ "submit"

prettyGuess :: GsRes -> H.Html
prettyGuess (GsRes (Gs l n) b t) = do
    H.li $ p $ do
            "Silly Sally likes "
            <> bigText l
            <> ", but not "
            <> bigText n
            <> ". "
            <> prettyBool b
            <> " "
            <> prettyTime t

bigText :: Text -> H.Html
bigText t =
    H.span ! class_ "big" $ toHtml t

prettyBool :: Bool -> H.Html
prettyBool True =
    H.span ! class_ "true" $ "Correct"
prettyBool False =
    H.span ! class_ "false" $ "Wrong"

prettyTime :: UTCTime -> H.Html
prettyTime tm =
    H.span ! class_ "time" $ do
        toHtml $ formatTime defaultTimeLocale "%D %R UST" tm
