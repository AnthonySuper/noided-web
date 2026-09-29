{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | JSON serialization of 'FormErrors'.
--
-- This lives apart from "Noided.Form.HKD.Internal.Type.FormErrors" because it needs the label and
-- error-evidence classes, which are defined in terms of 'FormErrors'.
--
-- Every error node is an object with a @base@ array of validation errors (see the 'ToJSON' instance
-- of 'ValidationErrors'). Subform nodes also have a @fields@ object keyed by field name, and list nodes
-- have an @items@ object keyed by list index. Children without any errors are omitted.
module Noided.Form.HKD.Internal.Json () where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.HKD
import Data.IntMap qualified as IM
import GHC.Generics ((:*:) (..))
import Noided.Form.HKD.Internal.Class
import Noided.Form.HKD.Internal.Type.FormErrors
import Noided.Form.HKD.Internal.Type.FormLabel
import Noided.Validation

instance (HasErrorEvidence field, HasFormLabelInner field) => ToJSON (FormErrors field) where
  toJSON = snd . formErrorsJSON hasErrors formLabelInner

-- | Convert errors to JSON, also reporting whether there were any errors at all.
formErrorsJSON :: HasErrors field -> FormLabelInner field -> FormErrors field -> (Bool, Value)
formErrorsJSON evidence label errs =
  case (evidence, label) of
    (InputHasErrors, InputLabelInner) ->
      (anyErrors errs.baseErrors, object ["base" .= errs.baseErrors])
    (SubformHasErrors innerEvidence, SubformLabelInner labels) ->
      case errs of
        SubformErrors base inner ->
          let fields =
                ffoldMap
                  ( \(FormLabel name innerLabel :*: (innerEv :*: innerErrs)) ->
                      keepErrors (Key.fromText name) (formErrorsJSON innerEv innerLabel innerErrs)
                  )
                  (fzipWith (:*:) labels (fzipWith (:*:) innerEvidence inner))
           in (anyErrors base || not (null fields), object ["base" .= base, "fields" .= object fields])
    (ListHasErrors innerEvidence, ListLabelInner innerLabel) ->
      case errs of
        ListErrors base inner ->
          let items =
                concatMap
                  (\(i, e) -> keepErrors (Key.fromString $ show i) (formErrorsJSON innerEvidence innerLabel e))
                  (IM.toList inner)
           in (anyErrors base || not (null items), object ["base" .= base, "items" .= object items])
  where
    anyErrors = not . nullErrors
    keepErrors key (has, value)
      | has = [key .= value]
      | otherwise = []
