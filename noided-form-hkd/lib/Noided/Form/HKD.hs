{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Form.HKD
  ( HKDForm (..),
    parseForm,
    renderForm,
    validateForm,

    -- ** Deriving helpers
    GHKDFormLenses,
    ghkdFormLenses,
    GHKDFormLabels,
    ghkdFormLabels,
    GHKDFormHasErrors,
    ghkdFormHasErrors,
    GHKDFormEmpty,
    gemptyForm,

    -- * Re-Exported Types
    module Noided.Form.HKD.Type,
  )
where

import Data.HKD
import GHC.Generics
import Noided.Form
import Noided.Form.HKD.Internal.Class
import Noided.Form.HKD.Internal.GEmptyForm
import Noided.Form.HKD.Internal.Parse
import Noided.Form.HKD.Internal.Render
import Noided.Form.HKD.Internal.Type.FormLabel (FormLabelInner (SubformLabelInner))
import Noided.Form.HKD.Internal.Validate
import Noided.Form.HKD.Type

-- | Parse a form from some form input.
parseForm :: (HKDForm t) => FormSubmission MultipartFormData -> t FormInput
parseForm input = ffmap (`parseForm'` input) hkdFormLabels

-- | Validate a form using a validator.
validateForm ::
  ( Monad m,
    HKDForm subform
  ) =>
  FormValidator m (SubformField subform) ->
  subform FormInput ->
  m (Either (FormErrors (SubformField subform)) (subform FormResult))
validateForm = validateHKDFormI hkdFormLenses hkdFormHasErrors

-- | Render a form using a renderer.
renderForm ::
  (Monad m, HKDForm subform) =>
  FormRenderer m (SubformField subform) ->
  subform FormInput ->
  FormErrors (SubformField subform) ->
  m ()
renderForm renderer input errs =
  renderHKDField' (ctx :*: renderer :*: SubformLabelInner hkdFormLabels)
  where
    ctx = RenderContext {key = emptyCanonicalKey, input = SubformInput input, errors = errs}
