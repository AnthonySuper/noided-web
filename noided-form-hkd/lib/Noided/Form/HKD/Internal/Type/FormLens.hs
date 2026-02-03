{-# LANGUAGE PatternSynonyms #-}

module Noided.Form.HKD.Internal.Type.FormLens
  ( FormLens (InputLens, SubformLens, ListLens),
    baseLens,
    formLensReflex,
    elementLens,
  )
where

import Data.HKD
import Data.Kind
import Noided.Form.HKD.Internal.Type.HKDFieldType
import Optics.Core

-- | Form lenses of a field.
type FormLensInner :: HKDFieldType -> Type
data FormLensInner field where
  InputLensInner :: FormLensInner (InputField t)
  SubformLensInner :: subform (FormLens subform) -> FormLensInner (SubformField subform)
  ListLensInner :: FormLens (Element field) field -> FormLensInner (ListField field)

type FormLens :: ((HKDFieldType -> Type) -> Type) -> HKDFieldType -> Type
data FormLens overallForm inner where
  FormLens ::
    (forall wrapper. Lens' (overallForm wrapper) (wrapper field)) ->
    FormLensInner field ->
    FormLens overallForm field

elementLens :: Lens (Element a1 f1) (Element a2 f2) (f1 a1) (f2 a2)
elementLens = lens (\(Element e) -> e) (\_ e -> Element e)

formLensReflex :: FormLens overallForm field -> FormLens (Element field) field
formLensReflex (FormLens _ e) =
  FormLens elementLens e

baseLens :: FormLens overallForm field -> Lens' (overallForm wrapper) (wrapper field)
baseLens (FormLens l _) = l

pattern InputLens :: () => (field ~ InputField t) => (forall wrapper. Lens' (overallForm wrapper) (wrapper field)) -> FormLens overallForm field
pattern InputLens l = FormLens l InputLensInner

pattern SubformLens ::
  () =>
  (field ~ SubformField subform) =>
  (forall wrapper. Lens' (overallForm wrapper) (wrapper field)) ->
  subform (FormLens subform) ->
  FormLens overallForm field
pattern SubformLens l is = FormLens l (SubformLensInner is)

pattern ListLens ::
  () =>
  (field ~ ListField inner) =>
  (forall wrapper. Lens' (overallForm wrapper) (wrapper field)) ->
  FormLens (Element inner) inner ->
  FormLens overallForm field
pattern ListLens l lf = FormLens l (ListLensInner lf)

{-# COMPLETE InputLens, SubformLens, ListLens #-}
