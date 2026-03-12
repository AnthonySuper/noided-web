{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Form.Render.CreateItem where

import Control.Monad (when)
import Lucid
import Noided.Form.HKD
import Noided.Web.Html.FormRender
import OptBeer.Form.Type.CreateItem

-- | A renderer for the item creation form.
createItemRenderer ::
  ( FetchMessages m,
    FetchHtmlFormatters m
  ) =>
  HtmlFormRendererT m (SubformField CreateItemF)
createItemRenderer =
  fieldWrapModelName "CreateItem" $
    wrapField baseErrorWrapper (subformField createItemRendererT)
  where
    baseErrorWrapper inner = do
      _ <- ul_ [class_ "form-base-errors"] $
        renderBaseErrors (li_ [class_ "form-base-error"])
      inner

createItemRendererT ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  CreateItemF (FormRenderer (HtmlFormT m))
createItemRendererT =
  CreateItem
    { name = textField,
      description = textField,
      defaultUnit = unitSelectField
    }
  where
    textField = formField $ fieldWrapper $ renderInputTag [class_ "form-field-input", type_ "text"]

    unitSelectField = formField $ fieldWrapper $ renderEnumSelectTag [class_ "form-field-input"]

    fieldWrapper inputAct = div_ [class_ "form-field-wrapper"] $ do
      hasError <- fieldHasError
      when hasError $ do
        ul_ [class_ "form-field-errors"] $
          renderFieldErrors (li_ [class_ "form-field-error"])
      renderLabelTag [class_ "form-field-label"]
      inputAct
