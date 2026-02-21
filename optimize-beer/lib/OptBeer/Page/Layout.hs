{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Page.Layout where

import Control.Monad (forM_)
import Control.Monad.Reader.Class
import Data.Text (Text)
import Lucid
import OptBeer.Page.Type
import Noided.Web.Effect.FrontendAssets

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
    main_ [id_ "overall-main"] inner
