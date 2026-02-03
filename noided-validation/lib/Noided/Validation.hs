-- |
-- Module: Noided.Validation
-- Description: Unified public API for the noided-validation library
--
-- This module provides a convenient unified interface to the noided-validation
-- library, re-exporting all the important functionality.
--
-- = Quick Start
--
-- Import this module to get access to all validation functionality:
--
-- @
-- import Noided.Validation
-- @
--
-- = Core Types
--
-- The library is built around the 'ValidatorT' monad transformer, which allows
-- you to accumulate both fatal and non-fatal validation errors.
--
-- = Usage Example
--
-- @
-- import Noided.Validation
-- import GHC.Generics (Generic)
--
-- -- Define custom error types
-- data AgeError = TooYoung | TooOld
--   deriving (Show, Eq, Ord, Generic)
--
-- instance ValidationError AgeError
--
-- -- Use custom errors in validation
-- validateAge :: Int -> Validator ()
-- validateAge age = do
--   require (age >= 0) TooYoung
--   check (age < 150) TooOld
--
-- result = runValidator (validateAge 25)
-- -- result: Right ()
-- @
module Noided.Validation
  ( -- * Validator Monad
    ValidatorT (..),
    Validator,
    runValidatorT,
    runValidatorTThese,
    runValidator,

    -- * Error Handling
    failNonfatal,
    failFatal,
    failFatalMany,

    -- * Validation Functions
    check,
    require,

    -- * Validation Error Types
    ValidationError (..),
    SomeValidationError (..),
    _SomeValidationError,

    -- * Error Collections
    ValidationErrors (..),
    nullErrors,
    hasError,
    singletonError,
    allErrors,
    errorsOfType,

    -- * Validation Combinators
    module Noided.Validation.Validate.Combinators,

    -- * Blank Validation
    module Noided.Validation.Validate.Blank,

    -- * Number Validation
    module Noided.Validation.Validate.Number,

    -- * Selection Validation
    module Noided.Validation.Validate.Selection,

    -- * Size Validation
    module Noided.Validation.Validate.Size,

    -- * Text Validation
    module Noided.Validation.Validate.Text,
  )
where

import Noided.Validation.Validate.Blank
import Noided.Validation.Validate.Combinators
import Noided.Validation.Validate.Number
import Noided.Validation.Validate.Selection
import Noided.Validation.Validate.Size
import Noided.Validation.Validate.Text
import Noided.Validation.ValidationError
import Noided.Validation.ValidationErrors
import Noided.Validation.Validator

