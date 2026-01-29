{-|
Module: Noided.Validation.Validate.Text
Description: Public API for text validation

This module provides validators for text values.
-}
module Noided.Validation.Validate.Text
  ( -- * Validators
    startsWith,
    endsWith,
    contains,
    notContains,

    -- * Error Types
    DoesNotStartWith (..),
    DoesNotEndWith (..),
    DoesNotContain (..),
    Contains (..),
  )
where

import Noided.Validation.Internal.Validate.Text
import Noided.Validation.Internal.ValidationError.Text
