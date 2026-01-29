{-|
Module: Noided.Validation.Validate.Selection
Description: Public API for selection validation

This module provides validators for checking if values are within or outside
allowed sets.
-}
module Noided.Validation.Validate.Selection
  ( -- * Validators
    oneOf,
    noneOf,

    -- * Error Types
    InvalidSelection (..),
    ForbiddenSelection (..),
  )
where

import Noided.Validation.Internal.Validate.Selection
import Noided.Validation.Internal.ValidationError.Selection
