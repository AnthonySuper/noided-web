{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Form.Render.CreateUser where

import Control.Monad (when)
import Lucid
import Noided.Form.HKD
import Noided.Web.Html.FormRender
import OptBeer.Form.Type.CreateUser

createUserRenderer ::
  ( FetchMessages m,
    FetchHtmlFormatters m
  ) =>
  HtmlFormRendererT m (SubformField CreateUserF)
createUserRenderer =
  fieldWrapModelName "CreateUser" $
    subformField createUserRendererT

createUserRendererT ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  CreateUserF (FormRenderer (HtmlFormT m))
createUserRendererT =
  CreateUser
    { name = textField,
      email = textField,
      confirmEmail = textField,
      password = passwordField,
      confirmPassword = passwordField
    }
  where
    textField = formField $ fieldWrapper $ renderInputTag [class_ "form-field-input", type_ "text"]
    passwordField = formField $ fieldWrapper $ renderInputTag [class_ "form-field-input", type_ "password"]

    fieldWrapper inputAct = div_ [class_ "form-field-wrapper"] $ do
      hasError <- fieldHasError
      when hasError $ do
        ul_ [class_ "form-field-errors"] $
          renderFieldErrors (li_ [class_ "form-field-error"])
      renderLabelTag [class_ "form-field-label"]
      inputAct
