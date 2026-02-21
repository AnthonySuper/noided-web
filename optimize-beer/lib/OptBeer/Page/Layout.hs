{-# LANGUAGE OverloadedRecordDot #-}

module OptBeer.Page.Layout where

import Control.Monad.Reader.Class
import Lucid
import OptBeer.Page.Type
import Optics

pageLayout :: HtmlT Page a -> HtmlT Page a
pageLayout inner = doctypehtml_ $ do
  env <- ask
  head_ $ do
    meta_ [charset_ "utf-8"]
    title_ [] $ do
      case env.pageTitle of
        Nothing -> "Optimize.beer"
        Just t -> do
          toHtml t
          " | Optimize.beer"
  body_ $ do
    main_ [id_ "overall-main"] inner
