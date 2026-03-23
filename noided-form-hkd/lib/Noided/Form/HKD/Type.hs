{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Form.HKD.Type
  ( -- * Data kind for HKD fields
    HKDFieldType (..),

    -- * Field inputs
    FieldInput (..),
    _FromForm,
    _FromTyped,
    _NotPresent,
    FormInput (..),
    _InputInput,
    _SubformInput,
    _ListInput,
    fieldInputTyped,
    fieldInputFromTyped,

    -- * Form Validators
    FormValidator,
    validateBase,
    validateBefore,
    validateInputRaw,
    validateInput,
    validateSubform,
    validateList,
    BadFormat (..),

    -- ** Changing monad of validation
    hoistFormValidator,

    -- * Form Results
    FormResult (..),
    _InputResult,
    _SubformResult,
    _ListResult,

    -- * Field errors
    FormErrors,
    FormInputErrors,
    inputErrors,
    FormSubformErrors,
    subformErrors,
    FormListErrors,
    listErrors,
    onlyBaseErrors,

    -- ** Traversals
    traverseFormErrors,
    formErrorSets,
    formErrors,
    traverseHkdFormErrors,
    hkdFormErrors,

    -- ** Evidence that a form has well-behaved errors
    HasErrors,
    emptyErrorsFromEvidence,
    inputHasErrors,
    subformHasErrors,
    listHasErrors,
    HasErrorEvidence (..),

    -- * Rendering forms
    FormRenderer,
    aroundRendering,
    renderInput,
    renderSubform,
    renderList,
    RenderingContext (..),
  )
where

import Data.HKD
import Data.Text (Text)
import GHC.Generics
import Noided.Form
import Noided.Form.HKD.Internal.Class
import Noided.Form.HKD.Internal.Type.FormErrors
import Noided.Form.HKD.Internal.Type.FormInput
import Noided.Form.HKD.Internal.Type.FormRenderer
import Noided.Form.HKD.Internal.Type.FormResult
import Noided.Form.HKD.Internal.Type.FormValidator
import Noided.Form.HKD.Internal.Type.HKDFieldType
import Noided.Validation
import Optics.Core

inputHasErrors :: HasErrors (InputField f)
inputHasErrors = InputHasErrors

subformHasErrors ::
  ( FTraversable subform,
    FRepeat subform,
    Monoid (subform FormErrors)
  ) =>
  subform HasErrors ->
  HasErrors (SubformField subform)
subformHasErrors = SubformHasErrors

listHasErrors :: HasErrors inner -> HasErrors (ListField inner)
listHasErrors = ListHasErrors

validateBase ::
  -- | Raw input, which you can perform validation on, and modify if needed.
  (FormInput field -> ValidatorT m (FormInput field)) ->
  FormValidator m field ->
  FormValidator m field
validateBase = BaseValidator

validateBefore ::
  (FormInput field -> ValidatorT m (FormValidator m field)) ->
  FormValidator m field
validateBefore = ValidateBefore

-- | Validate raw input.
validateInputRaw :: (FieldInput a -> ValidatorT m a) -> FormValidator m (InputField a)
validateInputRaw = InputValidator

newtype BadFormat = BadFormat {formatMsg :: Text}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

-- | Validates input.
-- Will throw a bad format error if input was not able to be parsed.
validateInput :: (FromFormSubmission MultipartFormData a, Monad m) => (a -> ValidatorT m a) -> FormValidator m (InputField a)
validateInput vf = validateInputRaw inner
  where
    inner fi = validateSingle fi >>= vf
    validateSingle = \case
      FromForm fv ->
        case fromFormSubmission (SubmissionValue fv) of
          Left e -> failFatal $ BadFormat e
          Right e -> return e
      NotPresent ->
        case fromFormSubmission @MultipartFormData SubmissionEmpty of
          Left e -> failFatal $ BadFormat e
          Right e -> return e
      FromTyped t -> return t

validateSubform :: (FTraversable subform) => subform (FormValidator m) -> FormValidator m (SubformField subform)
validateSubform = SubformValidator

validateList :: FormValidator m subfield -> FormValidator m (ListField subfield)
validateList = ListValidator

aroundRendering :: (forall a. RenderingContext field -> m a -> m a) -> FormRenderer m field -> FormRenderer m field
aroundRendering = AroundRendering

renderInput :: (RenderingContext (InputField field) -> m ()) -> FormRenderer m (InputField field)
renderInput = InputRenderer

renderSubform :: (FTraversable subform, FZip subform, Monoid (subform FormErrors)) => subform (FormRenderer m) -> FormRenderer m (SubformField subform)
renderSubform = SubformRenderer

renderList :: FormRenderer m field -> FormRenderer m (ListField field)
renderList = ListRenderer

traverseHkdFormErrors ::
  forall f form.
  (Applicative f, FTraversable form) =>
  (ValidationErrors -> f ValidationErrors) ->
  (form FormErrors) ->
  f (form FormErrors)
traverseHkdFormErrors f = ftraverse travInner
 where
   travInner :: forall field. FormErrors field -> f (FormErrors field)
   travInner = traverseFormErrors f

hkdFormErrors
  :: FTraversable form
  => Traversal (form FormErrors) (form FormErrors) ValidationErrors ValidationErrors
hkdFormErrors = traversalVL traverseHkdFormErrors
