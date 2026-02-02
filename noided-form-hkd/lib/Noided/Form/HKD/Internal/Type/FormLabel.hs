{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Form.HKD.Internal.Type.FormLabel (FormLabel (InputLabel, SubformLabel, ListLabel), FormLabelInner) where

import Data.Kind
import Data.String
import Data.Text (Text)
import Noided.Form.HKD.Internal.Type.HKDFieldType

-- | Inner labels for fields.
type FormLabelInner :: HKDFieldType -> Type
data FormLabelInner field where
  InputLabelInner :: FormLabelInner (InputField f)
  SubformLabelInner ::
    subform FormLabel -> FormLabelInner (SubformField subform)
  ListLabelInner ::
    FormLabel inner -> FormLabelInner (ListField inner)

deriving instance Show (FormLabelInner (InputField t))

deriving instance Eq (FormLabelInner (InputField t))

deriving instance Ord (FormLabelInner (InputField t))

deriving instance (Show (subform FormLabel)) => Show (FormLabelInner (SubformField subform))

deriving instance (Eq (subform FormLabel)) => Eq (FormLabelInner (SubformField subform))

deriving instance (Ord (subform FormLabel)) => Ord (FormLabelInner (SubformField subform))

deriving instance (Show (FormLabel inner)) => Show (FormLabelInner (ListField inner))

deriving instance (Eq (FormLabel inner)) => Eq (FormLabelInner (ListField inner))

deriving instance (Ord (FormLabel inner)) => Ord (FormLabelInner (ListField inner))

type FormLabel :: HKDFieldType -> Type
data FormLabel field = FormLabel Text (FormLabelInner field)

instance Show (FormLabel (InputField t)) where
  showsPrec d (FormLabel t InputLabelInner) =
    showParen (d > 10) $
      showString "InputLabel " . showsPrec 11 t

instance (Show (subform FormLabel)) => Show (FormLabel (SubformField subform)) where
  showsPrec d (FormLabel t (SubformLabelInner sf)) =
    showParen (d > 10) $
      showString "SubformLabel " . showsPrec 11 t . showString " " . showsPrec 11 sf

instance (Show (FormLabel inner)) => Show (FormLabel (ListField inner)) where
  showsPrec d (FormLabel t (ListLabelInner lf)) =
    showParen (d > 10) $
      showString "ListLabel " . showsPrec 11 t . showString " " . showsPrec 11 lf

deriving instance (Eq (FormLabelInner field)) => Eq (FormLabel field)

deriving instance (Ord (FormLabelInner field)) => Ord (FormLabel field)

instance (f ~ InputField t) => IsString (FormLabel f) where
  fromString = InputLabel . fromString

pattern InputLabel :: () => (field ~ InputField f) => Text -> FormLabel field
pattern InputLabel t = FormLabel t InputLabelInner

pattern SubformLabel :: () => (field ~ SubformField subform) => Text -> subform FormLabel -> FormLabel field
pattern SubformLabel t sf = FormLabel t (SubformLabelInner sf)

pattern ListLabel :: () => (field ~ ListField inner) => Text -> FormLabel inner -> FormLabel field
pattern ListLabel t lf = FormLabel t (ListLabelInner lf)

{-# COMPLETE InputLabel, SubformLabel, ListLabel #-}
