{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Form.Render.CreateSession where

import Control.Monad (when)
import Lucid
import Noided.Form.HKD
import Noided.Web.Html.FormRender
import OptBeer.Form.Type.CreateSession

-- | A renderer for the session creation (login) form.
createSessionRenderer ::
  ( FetchMessages m,
    FetchHtmlFormatters m
  ) =>
  HtmlFormRendererT m (SubformField CreateSessionF)
createSessionRenderer =
  fieldWrapModelName "CreateSession" $
    wrapField baseErrorWrapper (subformField createSessionRendererT)
  where
    -- Render base (subform-level) errors, such as invalid credential errors,
    -- before the actual form fields, using the same styling as field errors.
    baseErrorWrapper inner = do
      ul_ [class_ "form-field-errors"] $
        renderBaseErrors (li_ [class_ "form-field-error"])
      inner

createSessionRendererT ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  CreateSessionF (FormRenderer (HtmlFormT m))
createSessionRendererT =
  CreateSession
    { email = textField,
      password = passwordField
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
