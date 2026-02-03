{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Form.HKD.Internal.Type.FormErrors
  ( FormErrors (InputErrors, SubformErrors, ListErrors),
    FormInputErrors,
    inputErrors,
    FormSubformErrors,
    subformErrors,
    FormListErrors,
    listErrors,
    listInnerErrors,
    onlyBaseErrors,
    traverseFormErrors,
    formErrorSets,
    formErrors,
    HasErrors (..),
    emptyErrorsFromEvidence,
  )
where

import Data.HKD
import Data.IntMap qualified as IM
import Data.Kind
import GHC.Records
import Noided.Form.HKD.Internal.Type.HKDFieldType
import Noided.Validation
import Optics.Core

-- | Type of inner, non-base errors of a subform.
-- Note that there is *no case* for field inputs - all inputs in fields are reported in
-- the 'baseErrs' field of 'FormErrors'.
type FormErrorsInner :: HKDFieldType -> Type
data FormErrorsInner field where
  -- | Inner errors for a *subform*.
  SubformErrorsInner ::
    (FTraversable hkd, FZip hkd) =>
    hkd FormErrors ->
    FormErrorsInner (SubformField hkd)
  ListErrorsInner ::
    IM.IntMap (FormErrors field) ->
    FormErrorsInner (ListField field)

deriving instance (Show (hkd FormErrors)) => Show (FormErrorsInner (SubformField hkd))

deriving instance (Eq (hkd FormErrors)) => Eq (FormErrorsInner (SubformField hkd))

deriving instance (Ord (hkd FormErrors)) => Ord (FormErrorsInner (SubformField hkd))

deriving instance (Show (FormErrors field)) => Show (FormErrorsInner (ListField field))

deriving instance (Eq (FormErrors field)) => Eq (FormErrorsInner (ListField field))

deriving instance (Ord (FormErrors field)) => Ord (FormErrorsInner (ListField field))

-- | Dummy instance, this type is unpopulated.
instance Show (FormErrorsInner (InputField a)) where
  showsPrec = error "impossible"

-- | Dummy instance, this type is unpopulated.
instance Eq (FormErrorsInner (InputField a)) where
  (==) _ _ = True

-- | Dummy instance, this type is unpopulated.
instance Ord (FormErrorsInner (InputField a)) where
  compare _ _ = EQ

instance Semigroup (FormErrorsInner field) where
  ListErrorsInner m1 <> ListErrorsInner m2 = ListErrorsInner (IM.unionWith (<>) m1 m2)
  SubformErrorsInner s1 <> SubformErrorsInner s2 = SubformErrorsInner (fzipWith (<>) s1 s2)

traverseInnerValidationErrors :: (Applicative f) => (ValidationErrors -> f ValidationErrors) -> FormErrorsInner field -> f (FormErrorsInner field)
traverseInnerValidationErrors f (ListErrorsInner m) = ListErrorsInner <$> traverse (traverseFormErrors f) m
traverseInnerValidationErrors f (SubformErrorsInner s) = SubformErrorsInner <$> ftraverse (traverseFormErrors f) s

-- | Form errors of a given field.
data FormErrors field
  = OnlyBase ValidationErrors
  | BaseAndInner ValidationErrors !(FormErrorsInner field)

-- | Constructor for errors of input fields.
pattern InputErrors :: (field ~ InputField a) => ValidationErrors -> FormErrors field
pattern InputErrors baseErrors = OnlyBase baseErrors

type FormInputErrors is = FormErrors (InputField is)

{-# COMPLETE InputErrors #-}

inputErrors :: ValidationErrors -> FormInputErrors field
inputErrors = InputErrors

instance Eq (FormInputErrors is) where
  InputErrors e1 == InputErrors e2 = e1 == e2

instance Ord (FormInputErrors is) where
  compare (InputErrors e1) (InputErrors e2) = compare e1 e2

instance Show (FormInputErrors is) where
  showsPrec p (InputErrors e) =
    showParen (p > 10) $
      showString "InputErrors " . showsPrec 11 e

subformErrsAsPair :: (Monoid (field FormErrors)) => FormErrors (SubformField field) -> (ValidationErrors, field FormErrors)
subformErrsAsPair = \case
  OnlyBase e ->
    (e, mempty)
  BaseAndInner e (SubformErrorsInner se) -> (e, se)

type FormSubformErrors subform = FormErrors (SubformField subform)

instance (Eq (subform FormErrors), Monoid (subform FormErrors), FTraversable subform, FZip subform) => Eq (FormSubformErrors subform) where
  SubformErrors b1 f1 == SubformErrors b2 f2 = b1 == b2 && f1 == f2

instance (Ord (subform FormErrors), Monoid (subform FormErrors), FTraversable subform, FZip subform) => Ord (FormSubformErrors subform) where
  compare (SubformErrors b1 f1) (SubformErrors b2 f2) = case compare b1 b2 of
    EQ -> compare f1 f2
    other -> other

instance (Show (subform FormErrors), Monoid (subform FormErrors), FTraversable subform, FZip subform) => Show (FormSubformErrors subform) where
  showsPrec p (SubformErrors b f) =
    showParen (p > 10) $
      showString "SubformErrors " . showsPrec 11 b . showString " " . showsPrec 11 f

-- | Constructor for errors of subform fields.
pattern SubformErrors ::
  ( Monoid (subform FormErrors),
    FTraversable subform,
    FZip subform
  ) =>
  ValidationErrors ->
  subform FormErrors ->
  FormErrors (SubformField subform)
pattern SubformErrors baseErrors fieldErrors <- (subformErrsAsPair -> (baseErrors, fieldErrors))
  where
    SubformErrors baseErrors fieldErrors = BaseAndInner baseErrors (SubformErrorsInner fieldErrors)

subformErrors ::
  ( Monoid (subform FormErrors),
    FTraversable subform,
    FZip subform
  ) =>
  ValidationErrors ->
  subform FormErrors ->
  FormErrors (SubformField subform)
subformErrors = SubformErrors

{-# COMPLETE SubformErrors #-}

type FormListErrors inner = FormErrors (ListField inner)

instance (Eq (FormErrors inner)) => Eq (FormListErrors inner) where
  ListErrors b1 f1 == ListErrors b2 f2 = b1 == b2 && f1 == f2

instance (Ord (FormErrors inner)) => Ord (FormListErrors inner) where
  compare (ListErrors b1 f1) (ListErrors b2 f2) = case compare b1 b2 of
    EQ -> compare f1 f2
    other -> other

instance (Show (FormErrors inner)) => Show (FormListErrors inner) where
  showsPrec p (ListErrors b f) =
    showParen (p > 10) $
      showString "ListErrors " . showsPrec 11 b . showString " " . showsPrec 11 f

listErrsAsPair :: FormErrors (ListField field) -> (ValidationErrors, IM.IntMap (FormErrors field))
listErrsAsPair = \case
  OnlyBase e -> (e, IM.empty)
  BaseAndInner e (ListErrorsInner se) -> (e, se)

-- | Constructor for errors of list fields.
pattern ListErrors :: ValidationErrors -> IM.IntMap (FormErrors field) -> FormErrors (ListField field)
pattern ListErrors be fe <- (listErrsAsPair -> (be, fe))
  where
    ListErrors be fe = BaseAndInner be (ListErrorsInner fe)

listErrors :: ValidationErrors -> IM.IntMap (FormErrors field) -> FormListErrors field
listErrors = ListErrors

{-# COMPLETE ListErrors #-}

instance (f ~ ValidationErrors) => HasField "baseErrors" (FormErrors field) f where
  getField (OnlyBase be) = be
  getField (BaseAndInner be _) = be

instance (f ~ ValidationErrors) => HasField "innerErrors" (FormErrors (InputField a)) f where
  getField (OnlyBase be) = be

instance
  (Monoid (subform FormErrors), f ~ subform FormErrors) =>
  HasField "innerErrors" (FormErrors (SubformField subform)) f
  where
  getField (subformErrsAsPair -> (_, se)) = se

instance (f ~ IM.IntMap (FormErrors field)) => HasField "innerErrors" (FormErrors (ListField field)) f where
  getField (listErrsAsPair -> (_, se)) = se

instance
  (field ~ field') =>
  LabelOptic "baseErrors" A_Lens (FormErrors field) (FormErrors field') ValidationErrors ValidationErrors
  where
  labelOptic = lens getter' setter'
    where
      setter' :: FormErrors field -> ValidationErrors -> FormErrors field
      setter' (OnlyBase _) be = OnlyBase be
      setter' (BaseAndInner _ se) be = BaseAndInner be se
      getter' :: FormErrors field -> ValidationErrors
      getter' = \case
        OnlyBase be -> be
        BaseAndInner be _ -> be

instance
  ( Monoid (subform FormErrors),
    FTraversable subform,
    FRepeat subform,
    FTraversable subform',
    FRepeat subform'
  ) =>
  LabelOptic "innerErrors" A_Lens (FormErrors (SubformField subform)) (FormErrors (SubformField subform')) (subform FormErrors) (subform' FormErrors)
  where
  labelOptic = lens getter' setter'
    where
      setter' :: FormErrors (SubformField subform) -> subform' FormErrors -> FormErrors (SubformField subform')
      setter' (OnlyBase be) = BaseAndInner be . SubformErrorsInner
      setter' (BaseAndInner be _) = BaseAndInner be . SubformErrorsInner
      getter' :: FormErrors (SubformField subform) -> subform FormErrors
      getter' (subformErrsAsPair -> (_, se)) = se

instance LabelOptic "innerErrors" A_Lens (FormErrors (ListField field)) (FormErrors (ListField field')) (IM.IntMap (FormErrors field)) (IM.IntMap (FormErrors field')) where
  labelOptic = lens getter' setter'
    where
      setter' (OnlyBase be) = BaseAndInner be . ListErrorsInner
      setter' (BaseAndInner be _) = BaseAndInner be . ListErrorsInner
      getter' :: FormErrors (ListField field) -> IM.IntMap (FormErrors field)
      getter' (listErrsAsPair -> (_, ie)) = ie

listInnerErrors :: Lens (FormErrors (ListField field)) (FormErrors (ListField field')) (IM.IntMap (FormErrors field)) (IM.IntMap (FormErrors field'))
listInnerErrors = labelOptic @"innerErrors"

instance Semigroup (FormErrors field) where
  (OnlyBase l) <> (OnlyBase r) = OnlyBase (l <> r)
  (BaseAndInner lbe lse) <> (OnlyBase rbe) = BaseAndInner (lbe <> rbe) lse
  (OnlyBase lbe) <> (BaseAndInner rbe rse) = BaseAndInner (lbe <> rbe) rse
  (BaseAndInner lbe lse) <> (BaseAndInner rbe rse) =
    BaseAndInner (lbe <> rbe) (lse <> rse)

instance Monoid (FormErrors field) where
  mempty = OnlyBase mempty

onlyBaseErrors :: ValidationErrors -> FormErrors field
onlyBaseErrors errs = OnlyBase errs

traverseFormErrors :: (Applicative f) => (ValidationErrors -> f ValidationErrors) -> FormErrors field -> f (FormErrors field)
traverseFormErrors f (BaseAndInner b i) = BaseAndInner <$> f b <*> traverseInnerValidationErrors f i
traverseFormErrors f (OnlyBase be) = OnlyBase <$> f be

-- | Traverse over every error in an error set.
formErrorSets :: Traversal (FormErrors field) (FormErrors field) ValidationErrors ValidationErrors
formErrorSets = traversalVL traverseFormErrors

formErrors :: Optic A_Fold '[] (FormErrors field) (FormErrors field) SomeValidationError SomeValidationError
formErrors = formErrorSets % allErrors

-- | Evidence that a given field has proper error handling.
type HasErrors :: HKDFieldType -> Type
data HasErrors field where
  InputHasErrors :: HasErrors (InputField f)
  SubformHasErrors ::
    (FTraversable subform, FRepeat subform, Monoid (subform FormErrors)) =>
    subform HasErrors ->
    HasErrors (SubformField subform)
  ListHasErrors ::
    HasErrors inner ->
    HasErrors (ListField inner)

emptyErrorsFromEvidence :: HasErrors field -> FormErrors field
emptyErrorsFromEvidence = \case
  InputHasErrors -> mempty
  SubformHasErrors _ -> mempty
  ListHasErrors _ -> mempty
