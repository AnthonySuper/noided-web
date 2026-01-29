{-|
Module: Noided.Validation.Validate.Number
Description: Public API for number validation

This module provides validators for numeric values.
-}
module Noided.Validation.Validate.Number
  ( -- * Validators
    isOdd,
    isEven,

    -- * Error Types
    NotOdd (..),
    NotEven (..),
  )
where

import Noided.Validation.Internal.Validate.Number
import Noided.Validation.Internal.ValidationError.Number
