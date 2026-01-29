{-|
Module: Noided.Validation.Validate.Size
Description: Public API for size validation

This module provides validators for checking sizes and ranges of values.
-}
module Noided.Validation.Validate.Size
  ( -- * Length Validation
    lengthAtLeast,
    lengthAtMost,
    lengthBetween,

    -- * Value Range Validation
    valueAtLeast,
    valueAtMost,
    valueBetween,

    -- * Error Types
    TooLarge (..),
    TooSmall (..),
  )
where

import Noided.Validation.Internal.Validate.Size
import Noided.Validation.Internal.ValidationError.Size
