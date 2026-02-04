{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.FormRenderer where

import Control.Monad.Trans.Class
import Data.HKD
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
import Noided.Web.Html.Internal.Type.FormRendererT
import Optics.Core
import Web.HttpApiData

type HtmlFieldT field m = HtmlT (FieldRendererT field m)

type HtmlFormT m = HtmlT (FormRendererT m)

type HtmlFormRendererT m = FormRenderer (HtmlFormT m)

-- | Render an HKD form somewhere on the page.
renderFormT ::
  ( Monad n,
    HKDForm subform
  ) =>
  HtmlFormRendererT n (SubformField subform) ->
  subform FormInput ->
  FormErrors (SubformField subform) ->
  HtmlT n ()
renderFormT fr sf =
  runFormT . renderForm fr sf

htmlFieldToHtmlForm :: (Monad m) => HtmlFieldT field m a -> RenderingContext field -> HtmlFormT m a
htmlFieldToHtmlForm act ctx = hoistHtmlT (fieldRendererToForm ctx) act

htmlFormToHtmlField :: (Monad m) => HtmlFormT m a -> HtmlFieldT field m a
htmlFormToHtmlField = hoistHtmlT formRendererToField

htmlFieldContext :: (Monad m) => HtmlFieldT field m (FieldContext field)
htmlFieldContext = lift askFieldContext

htmlFieldLocal :: (Monad m) => (FieldContext field -> FieldContext field) -> HtmlFieldT field m a -> HtmlFieldT field m a
htmlFieldLocal f = hoistHtmlT (localFieldContext f)

-- | Set the model name for all fields rendered in a block.
fieldModelName :: (Monad m) => Text -> HtmlFieldT field m a -> HtmlFieldT field m a
fieldModelName mn = htmlFieldLocal (#baseContext % #modelNames .~ [mn])

fieldWrapModelName ::
  (Monad m) =>
  Text ->
  HtmlFormRendererT m field ->
  HtmlFormRendererT m field
fieldWrapModelName mn = wrapField (fieldModelName mn)

fieldWrapAddToId ::
  (Monad m) =>
  DomIdWriter ->
  HtmlFormRendererT m field ->
  HtmlFormRendererT m field
fieldWrapAddToId comp = wrapField (fieldAddToId comp)

-- | Add a suffix to all dom ids generated in in the inner block.
-- This is useful if you want to add some kind of \"identifier\" to the dom ids,
-- for use with something like morphdom.
fieldAddToId :: (Monad m) => DomIdWriter -> HtmlFieldT field m a -> HtmlFieldT field m a
fieldAddToId comp = htmlFieldLocal (#baseContext % #modifyDomId %~ (<> Endo (<> comp)))

-- | Render an individual field.
formField :: (Monad m) => HtmlFieldT (InputField field) m () -> HtmlFormRendererT m (InputField field)
formField = renderInput . htmlFieldToHtmlForm

subformField ::
  ( FTraversable subform,
    FZip subform,
    Monoid (subform FormErrors),
    Monad m
  ) =>
  subform (HtmlFormRendererT m) ->
  HtmlFormRendererT m (SubformField subform)
subformField = renderSubform

listField ::
  HtmlFormRendererT m field ->
  HtmlFormRendererT m (ListField field)
listField = renderList

wrapField ::
  (Monad m) =>
  (forall a. HtmlFieldT field m a -> HtmlFieldT field m a) ->
  HtmlFormRendererT m field ->
  HtmlFormRendererT m field
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
  ( HtmlFieldT (InputField input) m () ->
    HtmlFieldT (InputField input) m ()
  ) ->
  HtmlFieldT (InputField input) m ()
renderFieldErrors wrapper = htmlFieldContext >>= fieldContextRenderAttributeErrors wrapper

-- | Get a list of base errors of the field.
fieldBaseErrors :: (Monad m) => HtmlFieldT a m [SomeValidationError]
fieldBaseErrors =
  toListOf (#fieldContext % #errors % #baseErrors % allErrors)
    <$> htmlFieldContext

-- | Determine if this field has any errors, recursively.
fieldHasError :: (Monad m) => HtmlFieldT a m Bool
fieldHasError = has (#fieldContext % #errors % formErrors) <$> htmlFieldContext

-- | Get a unique ID for this field.
fieldId :: (Monad m) => HtmlFieldT a m DomIdWriter
fieldId = do
  ctx <- htmlFieldContext
  let baseId = ctx & asDomId . view (#fieldContext % #key)
  let transformedId = appEndo (ctx ^. #baseContext % #modifyDomId) baseId
  return transformedId

-- | Get the HTML name for this field.
fieldName :: (Monad m) => HtmlFieldT a m Text
fieldName = canonicalKeyToFieldName . view (#fieldContext % #key) <$> htmlFieldContext

inputFieldValue :: (Monad m) => HtmlFieldT (InputField a) m (FieldInput a)
inputFieldValue = view (#fieldContext % #input % _InputInput) <$> htmlFieldContext

-- | Attributes to set on any inputs.
-- Basically just the @name@ and @id@ attributes.
inputAttributesBase ::
  (Monad m) =>
  HtmlFieldT (InputField t) m Attributes
inputAttributesBase = do
  fid <- fieldId
  fname <- fieldName
  return $ name_ fname <> idFromDomId fid

fieldValueToInputAttributeValue' :: (t -> Text) -> FieldInput t -> Maybe Text
fieldValueToInputAttributeValue' mapTyped = \case
  NotPresent -> Nothing
  FromTyped t -> Just (mapTyped t)
  FromForm v ->
    case v of
      TextValue t -> Just t
      _ -> Nothing

inputValueText' :: (Monad m) => (t -> Text) -> HtmlFieldT (InputField t) m (Maybe Text)
inputValueText' f = fieldValueToInputAttributeValue' f <$> inputFieldValue

inputValueText :: (Monad m, ToHttpApiData t) => HtmlFieldT (InputField t) m (Maybe Text)
inputValueText = inputValueText' toQueryParam

inputValueAttribute' ::
  (Monad m) =>
  (t -> Text) ->
  HtmlFieldT (InputField t) m Attributes
inputValueAttribute' mapTyped = do
  foldMap
    value_
    <$> inputValueText' mapTyped

inputValueAttribute ::
  (Monad m, ToHttpApiData t) =>
  HtmlFieldT (InputField t) m Attributes
inputValueAttribute = inputValueAttribute' toQueryParam

inputAttributes' :: (Monad m) => (t -> Text) -> HtmlFieldT (InputField t) m Attributes
inputAttributes' f = (<>) <$> inputAttributesBase <*> inputValueAttribute' f

inputAttributes :: (Monad m, ToHttpApiData t) => HtmlFieldT (InputField t) m Attributes
inputAttributes = inputAttributes' toQueryParam

-- | Attributes to set on any labels.
-- Sets both a @for@ attribute, and an @id@ attribute (to the field id with "--label" appended)
labelAttributes :: (Monad m) => HtmlFieldT (InputField t) m Attributes
labelAttributes = do
  fid <- fieldId
  return $ for_ (domIdToText fid) <> idFromDomId (fid <> "label")

-- | Renders the (translated) field name of a field as text.
renderFieldName :: (FetchMessages m, FetchHtmlFormatters m) => HtmlFieldT field m ()
renderFieldName =
  htmlFieldContext >>= fieldContextRenderName

-- | Render a translated label tag, with some base attributes.
-- You can use these base attributes to set a class on the label, if you want.
renderLabelTag :: (FetchMessages m, FetchHtmlFormatters m) => [Attributes] -> HtmlFieldT (InputField t) m ()
renderLabelTag attrs = do
  labelAttrs <- labelAttributes
  label_ (attrs <> [labelAttrs]) $
    renderFieldName

-- | Renders an input with the given base attributes.
-- You can use those to add a class or what have you.
renderInputTag' ::
  (Monad m) =>
  -- | Transform an input value into text, if possible.
  (t -> Text) ->
  [Attributes] ->
  HtmlFieldT (InputField t) m ()
renderInputTag' f attrs = do
  iattrs <- inputAttributes' f
  input_ $ attrs <> [iattrs]

renderInputTag ::
  (Monad m, ToHttpApiData t) =>
  -- | Base attributes to add
  [Attributes] ->
  HtmlFieldT (InputField t) m ()
renderInputTag = renderInputTag' toQueryParam

renderTextareaTag' ::
  (Monad m) =>
  -- | Transform an input value to text in the textarea
  (t -> Text) ->
  -- | Base attributes to add
  [Attributes] ->
  HtmlFieldT (InputField t) m ()
renderTextareaTag' f attrs = do
  iattrs <- inputAttributesBase
  val <- foldMap id <$> inputValueText' f
  textarea_ (attrs <> [iattrs]) $ toHtml val

renderTextareaTag ::
  (Monad m) =>
  -- | Base attributes to add
  [Attributes] ->
  -- | Rendered textarea
  HtmlFieldT (InputField Text) m ()
renderTextareaTag = renderTextareaTag' id
