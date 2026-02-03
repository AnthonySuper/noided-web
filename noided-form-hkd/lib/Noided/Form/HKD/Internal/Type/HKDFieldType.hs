{-# LANGUAGE UndecidableInstances #-}

module Noided.Form.HKD.Internal.Type.HKDFieldType where

import Data.GADT.Compare
import Data.Kind
import Data.Type.Equality
import Type.Reflection

type HKDFieldType :: Type
data HKDFieldType where
  -- | A single input field in a given content type.
  InputField ::
    -- | Resulting type, after validation
    Type ->
    HKDFieldType
  -- | A subform, of some other HKD form.
  SubformField ::
    -- | Subform type
    ((HKDFieldType -> Type) -> Type) ->
    HKDFieldType
  -- | A list of fields.
  ListField ::
    -- | Elements of the list
    HKDFieldType ->
    HKDFieldType

type HKDFieldTypeSing :: HKDFieldType -> Type
data HKDFieldTypeSing inputType where
  InputFieldSing :: TypeRep t -> HKDFieldTypeSing (InputField t)
  SubformFieldSing :: TypeRep t -> HKDFieldTypeSing (SubformField t)
  ListFieldSing :: HKDFieldTypeSing inner -> HKDFieldTypeSing (ListField inner)


type KnownHKDFieldType :: HKDFieldType -> Constraint
class KnownHKDFieldType hkdType where
  hkdFieldTypeS :: HKDFieldTypeSing hkdType

instance (Typeable t) => KnownHKDFieldType (InputField t) where
  hkdFieldTypeS = InputFieldSing typeRep

instance (Typeable t) => KnownHKDFieldType (SubformField t) where
  hkdFieldTypeS = SubformFieldSing typeRep

instance (KnownHKDFieldType inner) => KnownHKDFieldType (ListField inner) where
  hkdFieldTypeS = ListFieldSing hkdFieldTypeS

instance GEq HKDFieldTypeSing where
  geq (InputFieldSing t1) (InputFieldSing t2) = do
    Refl <- testEquality t1 t2
    return Refl
  geq (SubformFieldSing t1) (SubformFieldSing t2) = do
    Refl <- testEquality t1 t2
    return Refl
  geq (ListFieldSing i1) (ListFieldSing i2) = do
    Refl <- geq i1 i2
    return Refl
  geq _ _ = Nothing

instance GCompare HKDFieldTypeSing where
  gcompare (InputFieldSing t1) (InputFieldSing t2) =
    case testEquality t1 t2 of
      Just Refl -> GEQ
      Nothing -> case compare (SomeTypeRep t1) (SomeTypeRep t2) of
        LT -> GLT
        GT -> GGT
        EQ -> error "Impossible: TypeReps equal but not GEQ"
  gcompare (InputFieldSing _) _ = GLT
  gcompare _ (InputFieldSing _) = GGT
  gcompare (SubformFieldSing t1) (SubformFieldSing t2) =
    case testEquality t1 t2 of
      Just Refl -> GEQ
      Nothing -> case compare (SomeTypeRep t1) (SomeTypeRep t2) of
        LT -> GLT
        GT -> GGT
        EQ -> error "Impossible: TypeReps equal but not GEQ"
  gcompare (SubformFieldSing _) _ = GLT
  gcompare _ (SubformFieldSing _) = GGT
  gcompare (ListFieldSing i1) (ListFieldSing i2) =
    case gcompare i1 i2 of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT

instance TestEquality HKDFieldTypeSing where
  testEquality = geq
