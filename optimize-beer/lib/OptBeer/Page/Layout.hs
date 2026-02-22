{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Page.Layout where

import Control.Monad (forM_)
import Control.Monad.Reader.Class
import Data.Text (Text)
import Lucid
import Noided.Web.Effect.FrontendAssets
import Noided.Web.Html
import OptBeer.DB.Table.Actor
import OptBeer.Page.Type

pageHeader :: HtmlT Page ()
pageHeader = header_ [id_ "overall-header"] $ do
  h1_ [id_ "header-name"] $
    a_ [href_ "/"] "Optimize.beer"
  r <- ask
  case r.currentActor of
    Nothing ->
      a_ [href_ "/sessions/new", id_ "header-login"] $
        renderTranslated ["layout.login"] mempty
    Just actor ->
      span_ [id_ "header-actor"] $
        toHtml actor.name

pageLayout :: HtmlT Page a -> HtmlT Page a
pageLayout inner = doctypehtml_ $ do
  env <- ask
  let AssetLinks jsFiles cssFiles = env.assets
  head_ $ do
    meta_ [charset_ "utf-8"]
    forM_ jsFiles $ \src ->
      script_ [type_ "module", src_ src] ("" :: Text)
    forM_ cssFiles $ \href ->
      link_ [rel_ "stylesheet", href_ href]
    title_ [] $ do
      case env.pageTitle of
        Nothing -> "Optimize.beer"
        Just t -> do
          toHtml t
          " | Optimize.beer"
  body_ $ do
    pageHeader
    main_ [id_ "overall-main"] inner
