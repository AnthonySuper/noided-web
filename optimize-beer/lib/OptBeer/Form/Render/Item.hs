{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Form.Render.Item where

import Lucid
import Noided.Form.HKD
import Noided.Web.Html.FormRender
import OptBeer.Form.Render.Base
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
