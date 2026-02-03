{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.FormRenderer where

import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Semigroup
import Data.Text (Text)
import Lucid
import Lucid.Base
import Noided.Form
import Noided.Form.HKD
import Noided.Validation
import Noided.Web.Html.Internal.Class.DomId
import Noided.Web.Html.Internal.Class.FetchHtmlFormatters
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Html.Internal.Type.DomIdWriter
import Noided.Web.Html.Internal.Type.FormContext
import Noided.Web.Html.Internal.Type.FormContextT
import Optics.Core

type HtmlFieldT field m = HtmlT (FieldRendererT field m)

type HtmlFormT m = HtmlT (FormContextT m)

htmlFieldToHtmlForm :: (Monad m) => HtmlFieldT field m a -> RenderingContext field -> HtmlFormT m a
htmlFieldToHtmlForm act ctx = hoistHtmlT (fieldRendererToForm ctx) act

htmlFormToHtmlField :: (Monad m) => HtmlFormT m a -> HtmlFieldT field m a
htmlFormToHtmlField = hoistHtmlT formRendererToField

htmlFieldContext :: (Monad m) => HtmlFieldT field m (FieldContext field)
htmlFieldContext = lift askFieldContext

htmlFieldLocal :: (Monad m) => (FieldContext field -> FieldContext field) -> HtmlFieldT field m a -> HtmlFieldT field m a
htmlFieldLocal f = hoistHtmlT (localFieldContext f)

fieldModelName :: (Monad m) => Text -> HtmlFieldT a1 m a2 -> HtmlFieldT a1 m a2
fieldModelName mn = htmlFieldLocal (#baseContext % #modelNames .~ [mn])

-- | Render an individual field.
formField :: (Monad m) => HtmlFieldT (InputField field) m () -> FormRenderer (HtmlFormT m) (InputField field)
formField = renderInput . htmlFieldToHtmlForm

wrapField ::
  (Monad m) =>
  (forall a. HtmlFieldT field m a -> HtmlFieldT field m a) ->
  FormRenderer (HtmlFormT m) field ->
  FormRenderer (HtmlFormT m) field
wrapField act = aroundRendering $ \ctx b ->
  htmlFieldToHtmlForm (act $ htmlFormToHtmlField b) ctx

-- | Render base errors for this input or subform.
renderBaseErrors ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  -- | Wrap each individual error.
  -- Probably want to make these items of a list.
  (HtmlFieldT field m () -> HtmlFieldT field m ()) ->
  HtmlFieldT field m ()
renderBaseErrors wrapper = do
  htmlFieldContext >>= fieldContextRenderBaseErrors wrapper

-- | Render errors for this input.
renderFieldErrors ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  -- | Wrap each individual error (probably in a list item)
  ( HtmlT (FieldRendererT (InputField input) m) () ->
    HtmlT (FieldRendererT (InputField input) m) ()
  ) ->
  HtmlT (FieldRendererT (InputField input) m) ()
renderFieldErrors wrapper = htmlFieldContext >>= fieldContextRenderAttributeErrors wrapper

-- | Get a list of base errors of the field.
fieldBaseErrors :: (Monad m) => HtmlT (FieldRendererT a m) [SomeValidationError]
fieldBaseErrors =
  toListOf (#fieldContext % #errors % #baseErrors % allErrors)
    <$> htmlFieldContext

-- | Determine if this field has any errors, recursively.
fieldHasError :: (Monad m) => HtmlT (FieldRendererT a m) Bool
fieldHasError = has (#fieldContext % #errors % formErrors) <$> htmlFieldContext

-- | Get a unique ID for this field.
fieldId :: (Monad m) => HtmlT (FieldRendererT a m) DomIdWriter
fieldId = do
  ctx <- htmlFieldContext
  let baseId = ctx & asDomId . view (#fieldContext % #key)
  let transformedId = appEndo (ctx ^. #baseContext % #modifyDomId) baseId
  return transformedId

-- | Get the HTML name for this field.
fieldName :: (Monad m) => HtmlT (FieldRendererT a m) Text
fieldName = canonicalKeyToFieldName . view (#fieldContext % #key) <$> htmlFieldContext

-- | Attributes to set on any inputs.
-- Basically just the @name@ and @id@ attributes.
inputAttributes ::
  (Monad m) =>
  HtmlT (FieldRendererT a m) Attributes
inputAttributes = do
  fid <- fieldId
  fname <- fieldName
  return $ name_ fname <> idFromDomId fid

-- | Renders the (translated) field name of a field as text.
renderFieldName :: (FetchMessages m, FetchHtmlFormatters m) => HtmlT (FieldRendererT field m) ()
renderFieldName =
  htmlFieldContext >>= formContextRenderName
