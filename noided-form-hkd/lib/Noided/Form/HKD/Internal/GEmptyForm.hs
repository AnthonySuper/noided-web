{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not_home #-}

module Noided.Form.HKD.Internal.GEmptyForm where

import GHC.Generics
import Noided.Form.HKD.Internal.Type.FormInput
import Noided.Form.HKD.Internal.Type.HKDFieldType

class GHKDFormEmpty rep where
  genericHKDFormEmpty :: rep ()

instance (GHKDFormEmpty l, GHKDFormEmpty r) => GHKDFormEmpty (l :*: r) where
  genericHKDFormEmpty = genericHKDFormEmpty :*: genericHKDFormEmpty

instance (GHKDFormEmpty i) => GHKDFormEmpty (M1 tag md i) where
  genericHKDFormEmpty = M1 genericHKDFormEmpty

instance
  (EmptyInput field) =>
  GHKDFormEmpty (Rec0 (FormInput field))
  where
  genericHKDFormEmpty = K1 emptyInput

class EmptyInput field where
  emptyInput :: FormInput field

instance EmptyInput (InputField f) where
  emptyInput = InputInput NotPresent

instance EmptyInput (ListField f) where
  emptyInput = ListInput mempty

instance
  (Generic (form FormInput), GHKDFormEmpty (Rep (form FormInput))) =>
  EmptyInput (SubformField form)
  where
  -- Uses 'gemptyForm' rather than 'hkdFormEmpty' to avoid a circular
  -- dependency on the 'HKDForm' class.  Since the default implementation
  -- of 'hkdFormEmpty' *is* 'gemptyForm', the behaviour is identical for
  -- any form that does not override 'hkdFormEmpty'.
  emptyInput = SubformInput gemptyForm

-- | Use generic machinery to derive an empty form.
--
-- You should basically always use this.
gemptyForm ::
  (Generic (form FormInput), GHKDFormEmpty (Rep (form FormInput))) =>
  form FormInput
gemptyForm = to genericHKDFormEmpty
