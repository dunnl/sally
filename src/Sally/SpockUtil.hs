{-|
   Module: Sally.Pages
   Description: Spock/Blaze utility function
   Maintainer: lawrence.dunn.iii@gmail.com
   License: MIT
-}

{-# language OverloadedStrings #-}

module Sally.SpockUtil where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as BL
import Web.Spock
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

blaze :: (MonadIO m) => Html -> ActionCtxT ctx m ()
blaze html = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    lazyBytes. renderHtml $ html
