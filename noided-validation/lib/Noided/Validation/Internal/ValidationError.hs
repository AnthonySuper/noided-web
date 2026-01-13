{-# LANGUAGE DefaultSignatures #-}

module Noided.Validation.Internal.ValidationError where

import Data.Aeson
import Data.Text (Text, pack)
import Data.Typeable
import GHC.Generics
import Noided.Translate
import Optics.Core

-- | Equivalent of 'Control.Exception.SomeException', but for validation errors.
data SomeValidationError where
  SomeValidationError :: (ValidationError e) => e -> SomeValidationError

instance Eq SomeValidationError where
  (SomeValidationError l) == (SomeValidationError r) =
    case cast r of
      Just r' -> l == r'
      Nothing -> False

instance Ord SomeValidationError where
  compare (SomeValidationError (l :: lhsT)) (SomeValidationError (r :: rhsT)) =
    case typeOf l `compare` typeOf r of
      EQ -> case cast r of
        Just r' -> compare l r'
        Nothing -> error "SomeValidationError: internal error, type match but cast failed"
      ord -> ord

instance Show SomeValidationError where
  showsPrec p (SomeValidationError e) = showsPrec p e

instance ToJSON SomeValidationError where
  toEncoding (SomeValidationError e) =
    pairs $
      "key" .= validationErrorKey e
        <> "params" .= validationErrorTranslateParams e
  toJSON (SomeValidationError e) =
    object
      [ "key" .= validationErrorKey e,
        "params" .= validationErrorTranslateParams e
      ]

_SomeValidationError :: (ValidationError err) => Prism' SomeValidationError err
_SomeValidationError = prism' toSomeValidationError fromSomeValidationError

-- | Class for validation errors.
-- This is very similar to 'Control.Exception.Exception', but with the added ability to
-- give validation errors a unique name and translation params.
class (Typeable e, Show e, Ord e) => ValidationError e where
  -- | Cast to some validation error. You basically never need to overwrite this.
  toSomeValidationError :: e -> SomeValidationError
  toSomeValidationError = SomeValidationError

  -- | Cast from some validation error back to this error type.
  -- This is useful if the validation errors have some kind of subtyping relationship.
  fromSomeValidationError :: SomeValidationError -> Maybe e
  fromSomeValidationError (SomeValidationError e) = cast e

  -- | A unique name for this validation error.
  -- As much as possible this should be globally unique.
  -- This will eventually be used in JSON serialization.
  validationErrorKey :: e -> Text
  default validationErrorKey :: (Typeable e) => e -> Text
  validationErrorKey = pack . tyConName . typeRepTyCon . typeOf

  -- | Convert a validation error to translation params.
  --
  -- Most often the thing you want to do with a validation error is to display some informative error message to the user.
  -- This method allows you to get the extra information required to do that.
  validationErrorTranslateParams :: e -> TranslateParams
  default validationErrorTranslateParams :: (Generic e, GAsTranslateParams (Rep e)) => e -> TranslateParams
  validationErrorTranslateParams = gasTranslateParams

instance ValidationError SomeValidationError where
  toSomeValidationError = id
  fromSomeValidationError = Just
  validationErrorKey (SomeValidationError e) = validationErrorKey e
  validationErrorTranslateParams (SomeValidationError e) = validationErrorTranslateParams e
