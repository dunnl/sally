{-# language OverloadedStrings #-}
{-# language ExplicitForAll #-}
{-# language GADTs #-}

module Sally.SpockUtil where

import Web.Spock hiding (text)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

blaze html = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    lazyBytes. renderHtml $ html
