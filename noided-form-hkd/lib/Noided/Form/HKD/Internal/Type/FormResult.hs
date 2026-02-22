{-# LANGUAGE UndecidableInstances #-}

module Noided.Form.HKD.Internal.Type.FormResult where

import Data.Kind
import Data.Sequence qualified as Seq
import GHC.Records
import Noided.Form.HKD.Internal.Type.HKDFieldType
import Optics.Core

type FormResult :: HKDFieldType -> Type
data FormResult field where
  InputResult :: t -> FormResult (InputField t)
  SubformResult :: subform FormResult -> FormResult (SubformField subform)
  ListResult ::
    Seq.Seq (FormResult inner) ->
    FormResult (ListField inner)

instance (v ~ t) => HasField "val" (FormResult (InputField t)) v where
  getField (InputResult r) = r

_InputResult :: Iso' (FormResult (InputField t)) t
_InputResult = iso (\(InputResult r) -> r) InputResult

_SubformResult :: Iso' (FormResult (SubformField subform)) (subform FormResult)
_SubformResult = iso (\(SubformResult sr) -> sr) SubformResult

_ListResult :: Iso' (FormResult (ListField inner)) (Seq.Seq (FormResult inner))
_ListResult = iso (\(ListResult ls) -> ls) ListResult
