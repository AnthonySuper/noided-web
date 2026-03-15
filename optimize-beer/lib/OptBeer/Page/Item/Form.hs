{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Page.Item.Form where

import Data.Text (Text)
import Lucid
import Noided.Form.HKD
import Noided.Translate (MessageKey)
import Noided.Web.Html (renderTranslated)
import Noided.Web.Html.FormRender (renderFormT)
import OptBeer.Form.Render.Item (itemRenderer)
import OptBeer.Form.Type.Item (ItemFormF)
import OptBeer.Page.Type

itemFormPage :: [MessageKey] -> [MessageKey] -> Text -> ItemFormF FormInput -> FormErrors (SubformField ItemFormF) -> HtmlT Page ()
itemFormPage titleKeys buttonKeys formAction input errs =
  div_ [class_ "item-form-container"] $ do
    h1_ $ renderTranslated titleKeys mempty

    form_ [method_ "post", action_ formAction, class_ "form", data_ "framelike" "true"] $ do
      renderFormT itemRenderer input errs
      div_ [class_ "form-buttons"] $
        button_ [class_ "button", type_ "submit"] $
          renderTranslated buttonKeys mempty
