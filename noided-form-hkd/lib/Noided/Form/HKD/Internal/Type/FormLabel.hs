{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Form.HKD.Internal.Type.FormLabel
  ( FormLabel (..),
    FormLabelInner (..),
  )
where

import Data.HKD
import Data.Kind
import Data.String
import Data.Text (Text)
import Noided.Form.HKD.Internal.Type.HKDFieldType

-- | Inner labels for fields.
type FormLabelInner :: HKDFieldType -> Type
data FormLabelInner field where
  InputLabelInner :: FormLabelInner (InputField f)
  SubformLabelInner ::
    (FFunctor subform) =>
    subform FormLabel ->
    FormLabelInner (SubformField subform)
  ListLabelInner ::
    FormLabelInner inner -> FormLabelInner (ListField inner)

deriving instance Show (FormLabelInner (InputField t))

deriving instance Eq (FormLabelInner (InputField t))

deriving instance Ord (FormLabelInner (InputField t))

deriving instance (Show (subform FormLabel)) => Show (FormLabelInner (SubformField subform))

deriving instance (Eq (subform FormLabel)) => Eq (FormLabelInner (SubformField subform))

deriving instance (Ord (subform FormLabel)) => Ord (FormLabelInner (SubformField subform))

deriving instance (Show (FormLabelInner inner)) => Show (FormLabelInner (ListField inner))

deriving instance (Eq (FormLabelInner inner)) => Eq (FormLabelInner (ListField inner))

deriving instance (Ord (FormLabelInner inner)) => Ord (FormLabelInner (ListField inner))

type FormLabel :: HKDFieldType -> Type
data FormLabel field = FormLabel Text (FormLabelInner field)

deriving instance (Show (FormLabelInner field)) => Show (FormLabel field)

deriving instance (Eq (FormLabelInner field)) => Eq (FormLabel field)

deriving instance (Ord (FormLabelInner field)) => Ord (FormLabel field)

instance (f ~ InputField t) => IsString (FormLabel f) where
  fromString s = FormLabel (fromString s) InputLabelInner
