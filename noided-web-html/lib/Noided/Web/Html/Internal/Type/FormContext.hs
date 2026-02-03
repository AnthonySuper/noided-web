{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.Type.FormContext where

import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics
import Lucid.Base
import Noided.Form
import Noided.Form.HKD
import Noided.Translate
import Noided.Validation
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Html.Internal.Translate
import Optics.Core

newtype FormContextBase = FormCtxBase {modelNames :: [Text]}
  deriving (Show, Generic)

data FormContext field
  = FormCtx
  { baseContext :: !FormContextBase,
    fieldContext :: RenderingContext field
  }
  deriving (Generic)

formContextFieldNames :: FormContext field -> [Text]
formContextFieldNames =
  toListOf $
    #fieldContext
      % #key
      % #getFormCanonicalKey
      % _Snoc
      % _2
      % #_CanonicalObjectPiece

formContextModelNames :: FormContext field -> [Text]
formContextModelNames = view (#baseContext % #modelNames)

formContextAttributePaths :: FormContext field -> [MessageKey]
formContextAttributePaths ctx = modelKeys ++ unqualifiedKeys
  where
    unqualifiedKeys = do
      fieldName <- formContextFieldNames ctx
      ["form.attributes" <> textToMessageKey fieldName]
    modelKeys = do
      modelName <- formContextModelNames ctx
      fieldName <- formContextFieldNames ctx
      ["form" <> textToMessageKey modelName <> "attributes" <> textToMessageKey fieldName]
{-# INLINEABLE formContextAttributePaths #-}

formContextAttributeNames :: FormContext field -> [MessageKey]
formContextAttributeNames = fmap (<> "name") . formContextAttributePaths
{-# INLINEABLE formContextAttributeNames #-}

formContextRenderName' ::
  (FetchMessages m) =>
  FormContext field ->
  Map.Map Text (HtmlT m () -> HtmlT m ()) ->
  HtmlT m ()
formContextRenderName' ctx m =
  renderTranslated' m (formContextAttributeNames ctx) mempty

formContextRenderName :: (FetchMessages m) => FormContext field -> HtmlT m ()
formContextRenderName ctx = formContextRenderName' ctx mempty

formContextRenderError' ::
  (FetchMessages m, ValidationError e) =>
  Map.Map Text (HtmlT m () -> HtmlT m ()) ->
  FormContext field ->
  e ->
  HtmlT m ()
formContextRenderError' m ctx =
  renderErrorTranslatedWithBase' m errorKeys
  where
    errorKeys = errorCtxKeys ++ errorBaseKeys
    errorBaseKeys = ["form.errors"]
    errorCtxKeys =
      (<> "errors") <$> formContextAttributeNames ctx

formContextRenderErrorsOf ::
  ( JoinKinds A_Lens l k,
    Is k A_Fold,
    FetchMessages m,
    ValidationError a1,
    Applicative f
  ) =>
  Optic l is (FormErrors a2) (FormErrors a2) a1 a1 ->
  Map.Map Text (HtmlT m () -> HtmlT m ()) ->
  (HtmlT m () -> f r) ->
  FormContext a2 ->
  f ()
formContextRenderErrorsOf l m wrapper ctx =
  traverseOf_
    (#fieldContext % #errors % l)
    (wrapper . formContextRenderError' m ctx)
    ctx

formContextRenderBaseErrors' :: (FetchMessages m, Applicative f) => Map.Map Text (HtmlT m () -> HtmlT m ()) -> (HtmlT m () -> f r) -> FormContext a2 -> f ()
formContextRenderBaseErrors' = formContextRenderErrorsOf (#baseErrors % allErrors)

formContextRenderBaseErrors :: (FetchMessages m, Applicative f) => (HtmlT m () -> f r) -> FormContext a2 -> f ()
formContextRenderBaseErrors = formContextRenderBaseErrors' mempty

formContextRenderAllErrors' :: (FetchMessages m, Applicative f) => Map.Map Text (HtmlT m () -> HtmlT m ()) -> (HtmlT m () -> f r) -> FormContext a2 -> f ()
formContextRenderAllErrors' = formContextRenderErrorsOf formErrors

formContextRenderAllErrors :: (FetchMessages m, Applicative f) => (HtmlT m () -> f r) -> FormContext a2 -> f ()
formContextRenderAllErrors = formContextRenderAllErrors' mempty
