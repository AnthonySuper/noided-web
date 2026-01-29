{-|
Module: Noided.Validation.ValidationError
Description: Public API for validation errors

This module provides the public API for validation error types and
type classes.
-}
module Noided.Validation.ValidationError
  ( -- * Validation Error Type Class
    ValidationError (..),

    -- * Existential Error Type
    SomeValidationError (..),
    _SomeValidationError,
  )
where

import Noided.Validation.Internal.ValidationError
