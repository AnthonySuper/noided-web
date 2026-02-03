{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.Type.FormContext where

import Data.Map qualified as Map
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

formContextFieldNames :: FieldContext field -> [Text]
formContextFieldNames =
  toListOf $
    #fieldContext
      % #key
      % #getFormCanonicalKey
      % _Snoc
      % _2
      % #_CanonicalObjectPiece

formContextModelNames :: FieldContext field -> [Text]
formContextModelNames = view (#baseContext % #modelNames)

-- | Get paths for a form's fully-qualified attribute path.
-- So, if this form is for an attribute named @ foo @ of a model named @ User @,
-- this would return:
--
--     * @ form.User.attributes.foo @
formContextModelAttributePaths :: FieldContext field -> [MessageKey]
formContextModelAttributePaths ctx = do
  modelName <- formContextModelNames ctx
  fieldName <- formContextFieldNames ctx
  ["form" <> textToMessageKey modelName <> "attributes" <> textToMessageKey fieldName]
{-# INLINE formContextModelAttributePaths #-}

-- | Get paths for a form's unqualified attribute path.
-- So, if this form is for an attribute named @foo@ of a model named @User@, this returns:
--
--     * @ form.attributes.foo @
formContextBaseAttributePaths :: FieldContext field -> [MessageKey]
formContextBaseAttributePaths ctx = do
  fieldName <- formContextFieldNames ctx
  ["form.attributes" <> textToMessageKey fieldName]

formContextAttributePaths :: FieldContext field -> [MessageKey]
formContextAttributePaths ctx = modelKeys ++ unqualifiedKeys
  where
    unqualifiedKeys = formContextBaseAttributePaths ctx
    modelKeys = formContextModelAttributePaths ctx
{-# INLINEABLE formContextAttributePaths #-}

formContextAttributeNames :: FieldContext field -> [MessageKey]
formContextAttributeNames = fmap (<> "name") . formContextAttributePaths
{-# INLINEABLE formContextAttributeNames #-}

-- | Get translation prefixes to use for an attribute error, on an attribute form.
-- If this form is for an attribute named @foo@, with a model name of @User@, the following keys will be used as a prefix
-- (in order):
--
--     * @ form.User.attributes.foo.errors @
--     * @ form.User.errors @
--     * @ form.errors @
--     * @ errors @
formContextAttributeErrorTranslationPrefixes :: FieldContext field -> [MessageKey]
formContextAttributeErrorTranslationPrefixes _ = error "TODO: implement me"

-- | Get translation prefixes to use for a @base@ error on a form.
-- If this form is for a model named @User@, the following keys will be used as a prefix:
--
--     * @ form.User.base.errors s@
--     * @ form.errors @
--     * @ errors @
formContextBaseErrorTranslationPrefixes :: FieldContext field -> [MessageKey]
formContextBaseErrorTranslationPrefixes _ = error "TODO: implement me"

-- | Render a name in the form context.
--
-- Assuming this is a field named @foo@, with a model name of @User@, the following keys will be used to look up:
--
--     * @ form.User.attributes.foo.name @
--     * @ form.attributes.foo.name @
formContextRenderName ::
  ( FetchMessages m,
    FetchHtmlFormatters m
  ) =>
  FieldContext field ->
  HtmlT m ()
formContextRenderName ctx =
  renderTranslated (formContextAttributeNames ctx) mempty

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
    (formContextAttributeErrorTranslationPrefixes ctx)

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
    (formContextBaseErrorTranslationPrefixes ctx)

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
