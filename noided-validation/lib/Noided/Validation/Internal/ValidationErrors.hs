{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Validation.Internal.ValidationErrors where

import Data.Aeson
import Data.Set qualified as Set
import Noided.Validation.Internal.ValidationError
import Optics.Core

-- | A set of validation errors.
newtype ValidationErrors = MkValidationErrors {getValidationErrors :: Set.Set SomeValidationError}
  deriving (Show, Eq, Ord, ToJSON, Semigroup, Monoid) via (Set.Set SomeValidationError)

type instance Index ValidationErrors = SomeValidationError

instance Contains ValidationErrors where
  contains c = coercedTo @(Set.Set SomeValidationError) % contains c

nullErrors :: ValidationErrors -> Bool
nullErrors = Set.null . getValidationErrors

-- | Fold to access all errors in the set.
allErrors :: Fold ValidationErrors SomeValidationError
allErrors = coercedTo @(Set.Set SomeValidationError) % folded

errorsOfType :: forall e. (ValidationError e) => Fold ValidationErrors e
errorsOfType = allErrors % _SomeValidationError

-- | Determine if the set of errors contains a particular error or not.
hasError :: (ValidationError a) => ValidationErrors -> a -> Bool
hasError = flip (elemOf errorsOfType)

-- | Build a singleton error set from a given validation error.
singletonError :: (ValidationError e) => e -> ValidationErrors
singletonError = MkValidationErrors . Set.singleton . toSomeValidationError
