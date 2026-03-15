{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Form.Render.Item where

import Control.Monad (when)
import Lucid
import Noided.Form.HKD
import Noided.Web.Html.FormRender
import OptBeer.Form.Type.Item

-- | A renderer for the item form.
itemRenderer ::
  ( FetchMessages m,
    FetchHtmlFormatters m
  ) =>
  HtmlFormRendererT m (SubformField ItemFormF)
itemRenderer =
  fieldWrapModelName "Item" $
    wrapField baseErrorWrapper (subformField itemRendererT)
  where
    baseErrorWrapper inner = do
      _ <- ul_ [class_ "form-base-errors"] $
        renderBaseErrors (li_ [class_ "form-base-error"])
      inner

itemRendererT ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  ItemFormF (FormRenderer (HtmlFormT m))
itemRendererT =
  ItemForm
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
