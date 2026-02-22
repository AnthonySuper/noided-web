{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Form.Render.CreateOrganization where

import Control.Monad (when)
import Lucid
import Noided.Form.HKD
import Noided.Web.Html.FormRender
import OptBeer.Form.Type.CreateOrganization

-- | A renderer for the organization creation form.
createOrganizationRenderer ::
  ( FetchMessages m,
    FetchHtmlFormatters m
  ) =>
  HtmlFormRendererT m (SubformField CreateOrganizationF)
createOrganizationRenderer =
  fieldWrapModelName "CreateOrganization" $
    wrapField baseErrorWrapper (subformField createOrganizationRendererT)
  where
    baseErrorWrapper inner = do
      ul_ [class_ "form-base-errors"] $
        renderBaseErrors (li_ [class_ "form-base-error"])
      inner

createOrganizationRendererT ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  CreateOrganizationF (FormRenderer (HtmlFormT m))
createOrganizationRendererT =
  CreateOrganization
    { name = textField
    }
  where
    textField = formField $ fieldWrapper $ renderInputTag [class_ "form-field-input", type_ "text"]

    fieldWrapper inputAct = div_ [class_ "form-field-wrapper"] $ do
      hasError <- fieldHasError
      when hasError $ do
        ul_ [class_ "form-field-errors"] $
          renderFieldErrors (li_ [class_ "form-field-error"])
      renderLabelTag [class_ "form-field-label"]
      inputAct
