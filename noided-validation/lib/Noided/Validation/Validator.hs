-- |
-- Module: Noided.Validation.Validator
-- Description: Public API for validation monads
--
-- This module provides the public API for the validator monad transformer
-- and related types and functions.
module Noided.Validation.Validator
  ( -- * Validator Monad
    ValidatorT (..),
    Validator,
    runValidatorT,
    runValidator,
    runValidatorTThese,

    -- * Error Handling
    failNonfatal,
    failFatal,
    failFatalMany,

    -- * Validation Functions
    check,
    require,
  )
where

import Noided.Validation.Internal.Validator
