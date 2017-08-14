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

aboutHtml :: Html
aboutHtml =  do
    H.head $ do
        H.title "Silly Sally"
        includes
    H.body $ do
        H.div ! A.class_ "container" $ do
            navbar
            aboutContents
        
aboutContents :: Html
aboutContents = do
    H.div ! A.class_ "row" $ do
        H.div ! A.class_ "col-md-8 col-md-offset-2" $ do
            H.header $
                H.h1 $ do
                    "Silly Sally"
                    H.small ! A.class_ "small-header" $ 
                        "About"
            H.section $ do
                H.p $
                    "This application was my first excursion into web programming, with or without Haskell, \
                    \ and my first experience with Javascript. \
                    \ My aim at the time was to use as much self-written code as possible instead of \
                    \ relying on higher-abstraction libraries either in Haskell (Yesod, Persistent) \
                    \ or Javascript (jQuery, React). Nonetheless I wanted to built a moderately complex service \
                    \ with persistent storage, concurrency, and coordinated bidirectional communication between \
                    \ the frontend and backend."

                H.p $
                    "I used these libraries: Spock, Blaze, Websockets, sqlite-simple and (of course) Bootstrap"

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
        H.section ! A.class_ "col-md-8 col-md-offset-2 qa__header" $ do
            H.dl ! A.id "qa__list" $ do
                H.dt $ do
                    H.span ! A.class_ "qa__lead" $ "Q:"
                    H.div ! A.class_ "qa__content" $
                        "What is this?"
                H.dd $ do
                    H.span ! A.class_ "qa__lead" $ "A:"
                    H.div ! A.class_ "qa__content" $ do
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
--                H.li $ H.a ! href "data" $ "Data"

socketsDiv :: Html 
socketsDiv = do
    H.header "Program messages"
    H.hr
    H.div ! A.id "message__div" $ do
        H.ul ! A.id "message__list" ! A.class_ "message__list" $ ""

gameDiv :: View H.Html -> [GsRes] -> Html
gameDiv v gsrs = do
    H.header "Submit a guess"
    H.hr
    guessView v
    H.header ! class_ "guessTitle" $
        H.h2 "Last 8 guesses"
    H.ul ! A.id "game__list" ! A.class_ "game__list" $ do
        forM_ gsrs prettyGuess

guessForm :: (Monad m) => Form Html m Gs
guessForm = Gs
    <$> "likes"    .: D.text Nothing
    <*> "notlikes" .: D.text Nothing
    <*> "user"     .: D.text (Just "NoScript User")

guessView :: View H.Html -> H.Html
guessView view = do
    DB.form view "/" ! A.id "guess__form" $ do
        H.div ! class_ "line" $ do
            DB.label "likes" view "Silly Sally likes"
            H.div ! class_ "input" $ DB.inputText "likes" view
        H.div ! class_ "line" $ do
            DB.label "notlikes" view "But not"
            H.div ! class_ "input" $ DB.inputText "notlikes" view
        DB.inputSubmit "Submit" ! class_ "submit"

prettyGuess :: GsRes -> H.Html
prettyGuess (GsRes (Gs l n u) b t) = do
    H.li $ p $ do
            "Silly Sally likes "
            <> bigText l
            <> ", but not "
            <> bigText n
            <> ". "
            <> prettyBool b
            <> " Submitted by " 
            <> toHtml u
            <> " at "
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
