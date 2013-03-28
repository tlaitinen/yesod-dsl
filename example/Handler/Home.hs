{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
        aDomId <- lift newIdent
        addStylesheet $StaticR bootstrap_css
        addScript $StaticR app_js
        setTitle "yesod-generate-rest example"
