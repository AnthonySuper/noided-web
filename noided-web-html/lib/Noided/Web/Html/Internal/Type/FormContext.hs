{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.Type.FormContext where

import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import Lucid.Base
import Noided.Form
import Noided.Form.HKD
import Noided.Translate
import Noided.Validation
import Noided.Web.Html.Internal.Class.FetchHtmlFormatters
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Html.Internal.Translate
import Noided.Web.Html.Internal.Type.DomIdWriter
import Optics.Core

-- | Overall context for rendering an entire form.
data FormContext
  = FormCtxBase
  { modelNames :: [Text],
    modifyDomId :: Endo DomIdWriter
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (Generically FormContext)

data FieldContext field
  = FieldCtx
  { baseContext :: !FormContext,
    fieldContext :: RenderingContext field
  }
  deriving (Generic)

fieldContextFieldNames :: FieldContext field -> [Text]
fieldContextFieldNames =
  toListOf $
    #fieldContext
      % #key
      % #getFormCanonicalKey
      % _Snoc
      % _2
      % #_CanonicalObjectPiece

fieldContextModelNames :: FieldContext field -> [Text]
fieldContextModelNames = view (#baseContext % #modelNames)

-- | Get paths for a form's fully-qualified attribute path.
-- So, if this form is for an attribute named @ foo @ of a model named @ User @,
-- this would return:
--
--     * @ form.User.attributes.foo @
fieldContextModelAttributePaths :: FieldContext field -> [MessageKey]
fieldContextModelAttributePaths ctx = do
  modelName <- fieldContextModelNames ctx
  fieldName <- fieldContextFieldNames ctx
  ["form" <> textToMessageKey modelName <> "attributes" <> textToMessageKey fieldName]
{-# INLINE fieldContextModelAttributePaths #-}

-- | Get paths for a form's unqualified attribute path.
-- So, if this form is for an attribute named @foo@ of a model named @User@, this returns:
--
--     * @ form.attributes.foo @
fieldContextBaseAttributePaths :: FieldContext field -> [MessageKey]
fieldContextBaseAttributePaths ctx = do
  fieldName <- fieldContextFieldNames ctx
  ["form.attributes" <> textToMessageKey fieldName]

fieldContextAttributePrefixes :: FieldContext field -> [MessageKey]
fieldContextAttributePrefixes ctx = modelKeys ++ unqualifiedKeys
  where
    unqualifiedKeys = fieldContextBaseAttributePaths ctx
    modelKeys = fieldContextModelAttributePaths ctx
{-# INLINEABLE fieldContextAttributePrefixes #-}

fieldContextAttributeNameKeys :: FieldContext field -> [MessageKey]
fieldContextAttributeNameKeys = fmap (<> "name") . fieldContextAttributePrefixes
{-# INLINEABLE fieldContextAttributeNameKeys #-}

-- | Get translation prefixes to use for an attribute error, on an attribute form.
-- If this form is for an attribute named @foo@, with a model name of @User@, the following keys will be used as a prefix
-- (in order):
--
--     * @ form.User.attributes.foo.errors @
--     * @ form.User.errors @
--     * @ form.errors @
--     * @ errors @
fieldContextAttributeErrorTranslationPrefixes :: FieldContext field -> [MessageKey]
fieldContextAttributeErrorTranslationPrefixes ctx =
  specificKeys ++ modelKeys ++ ["form.errors", "errors"]
  where
    specificKeys = fmap (<> "errors") (fieldContextModelAttributePaths ctx)
    modelKeys = do
      modelName <- fieldContextModelNames ctx
      ["form" <> textToMessageKey modelName <> "errors"]

-- | Get translation prefixes to use for a @base@ error on a form.
-- If this form is for a model named @User@, the following keys will be used as a prefix:
--
--     * @ form.User.base.errors s@
--     * @ form.errors @
--     * @ errors @
fieldContextBaseErrorTranslationPrefixes :: FieldContext field -> [MessageKey]
fieldContextBaseErrorTranslationPrefixes ctx =
  modelKeys ++ ["form.errors", "errors"]
  where
    modelKeys = do
      modelName <- fieldContextModelNames ctx
      ["form" <> textToMessageKey modelName <> "base" <> "errors"]

-- | Render a name in the form context.
--
-- Assuming this is a field named @foo@, with a model name of @User@, the following keys will be used to look up:
--
--     * @ form.User.attributes.foo.name @
--     * @ form.attributes.foo.name @
fieldContextRenderName ::
  ( FetchMessages m,
    FetchHtmlFormatters m
  ) =>
  FieldContext field ->
  HtmlT m ()
fieldContextRenderName ctx =
  renderTranslated (fieldContextAttributeNameKeys ctx) mempty

-- | Render an error using a given form context.
--
-- Assuming this form context is for a field named @foo@ with a model name of @User@,
-- the following keys will be used to look up an error with a key of @Error@:
--
--     * @ form.User.attributes.foo.error.Error @
--     * @ form.User.errors.Error @
--     * @ form.errors.Error @
--     * @ errors.Error @
fieldContextRenderAttributeError ::
  ( FetchMessages m,
    FetchHtmlFormatters m,
    ValidationError e
  ) =>
  FieldContext field ->
  e ->
  HtmlT m ()
fieldContextRenderAttributeError ctx =
  renderErrorTranslatedWithPrefixes
    (fieldContextAttributeErrorTranslationPrefixes ctx)

-- | Render a single base error of a field.

---
-- Assuming this is a field named @foo@, with a model name of @User@,
-- the following keys will be used to look up an error named @Error@:
--
--     * @ form.User.base.errors.Error @
--     * @ form.errors.Error @
--     * @ errors.Error @
fieldContextRenderBaseError ::
  ( FetchMessages m,
    FetchHtmlFormatters m,
    ValidationError e
  ) =>
  FieldContext field ->
  e ->
  HtmlT m ()
fieldContextRenderBaseError ctx =
  renderErrorTranslatedWithPrefixes
    (fieldContextBaseErrorTranslationPrefixes ctx)

-- | Render the /base/ errors of a field.
--
-- These errors will be translated without an attribute name,
-- via 'fieldContextRenderBaseError'.
fieldContextRenderBaseErrors ::
  ( FetchMessages m,
    FetchHtmlFormatters m
  ) =>
  (HtmlT m () -> HtmlT m ()) ->
  FieldContext a ->
  HtmlT m ()
fieldContextRenderBaseErrors wrapper ctx =
  traverseOf_
    (#fieldContext % #errors % #baseErrors % allErrors)
    (wrapper . fieldContextRenderBaseError ctx)
    ctx

-- | Render attribute errors for an input field.
fieldContextRenderAttributeErrors ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  (HtmlT m () -> HtmlT m ()) ->
  FieldContext (InputField input) ->
  HtmlT m ()
fieldContextRenderAttributeErrors wrapper ctx =
  traverseOf_
    (#fieldContext % #errors % #baseErrors % allErrors)
    (wrapper . fieldContextRenderAttributeError ctx)
    ctx
