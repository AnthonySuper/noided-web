{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module OptBeer.Form.Render.Base where

import Control.Monad (when)
import Lucid
import Noided.Form.HKD
import Noided.Web.Html.FormRender

baseErrorWrapper ::
  (FetchHtmlFormatters m, FetchMessages m) =>
  HtmlFieldT field m a ->
  HtmlFieldT field m a
baseErrorWrapper inner = do
  ul_ [class_ "form-base-errors"] $ renderBaseErrors (li_ [class_ "form-base-error"])
  inner

fieldWrapper ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  HtmlFieldT (InputField inputType) m a ->
  HtmlFieldT (InputField inputType) m a
fieldWrapper inputAct =
  div_ [class_ "form-field-wrapper"] $ do
    hasError <- fieldHasError
    when hasError $ do
      ul_ [class_ "form-field-errors"] $
        renderFieldErrors (li_ [class_ "form-field-error"])
    renderLabelTag [class_ "form-field-label"]
    inputAct
