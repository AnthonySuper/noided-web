module Noided.Form.HKD.Internal.Type.FormValidator where

import Control.Monad.Morph
import Data.HKD
import Data.Kind
import Noided.Form.HKD.Internal.Type.FormInput
import Noided.Form.HKD.Internal.Type.HKDFieldType
import Noided.Validation

type FormValidator :: (Type -> Type) -> HKDFieldType -> Type
data FormValidator m field where
  -- | Perform *conditional validation* by doing something before a returning a validator.
  -- This is useful if you have conditional validations - the classic case here is having both a @password@ and
  -- a @confirmPassword@ field, in  which case validation of each field depends on the other value.
  --
  -- Any errors reported while building the validator will be reported as the form's base errors.
  ValidateBefore ::
    (FormInput field -> ValidatorT m (FormValidator m field)) ->
    FormValidator m field
  -- | Add *base validations* to a type.
  BaseValidator ::
    -- | Validate the entire input, potentially changing it along the way.
    -- If this runs with non-fatal errors, the inner validator will also be ran,
    -- and the errors will be combined at the end of the day.
    (FormInput field -> ValidatorT m (FormInput field)) ->
    FormValidator m field ->
    FormValidator m field
  -- | Validate a particular input.
  InputValidator ::
    (FieldInput a -> ValidatorT m a) ->
    FormValidator m (InputField a)
  -- | Validate every field of a subform.
  SubformValidator ::
    (FTraversable subform) =>
    subform (FormValidator m) ->
    FormValidator m (SubformField subform)
  -- | Validate every element of a list.
  ListValidator ::
    FormValidator m subfield ->
    FormValidator m (ListField subfield)

-- | Hoist a form validator into a new monad.
hoistFormValidator :: forall m n field. (Monad m, Monad n) => (forall res. m res -> n res) -> FormValidator m field -> FormValidator n field
hoistFormValidator f = \case
  ValidateBefore validateBefore ->
    ValidateBefore $ \inp -> do
      next <- hoist f (validateBefore inp)
      return $ hoistFormValidator f next
  BaseValidator validateBase validateInner ->
    BaseValidator
      (hoist f <$> validateBase)
      (hoistFormValidator f validateInner)
  InputValidator validateInput ->
    InputValidator (hoist f <$> validateInput)
  SubformValidator sfe ->
    SubformValidator (ffmap (hoistFormValidator f) sfe)
  ListValidator ls ->
    ListValidator (hoistFormValidator f ls)
