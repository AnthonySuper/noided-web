{-|
Module: Noided.Validation.ValidationErrors
Description: Public API for validation error collections

This module provides the public API for working with sets of validation errors.
-}
module Noided.Validation.ValidationErrors
  ( -- * Error Set Type
    ValidationErrors (..),

    -- * Query Functions
    nullErrors,
    hasError,

    -- * Construction
    singletonError,

    -- * Optics
    allErrors,
    errorsOfType,
  )
where

import Noided.Validation.Internal.ValidationErrors
