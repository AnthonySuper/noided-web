{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Validation.Internal.Validate.Size where

import Control.Monad (when)
import Data.Typeable
import Noided.Translate
import Noided.Validation.Internal.ValidationError.Size
import Noided.Validation.Internal.Validator

-- | Validate that a foldable has at least the given number of elements.
lengthAtLeast :: (Monad m, Foldable f) => Int -> f a -> ValidatorT m ()
lengthAtLeast minLen f =
  let len = length f
   in (when (len < minLen) $ failNonfatal $ TooSmall minLen)

-- | Validate that a foldable has at most the given number of elements.
lengthAtMost :: (Foldable t, Monad m) => Int -> t a -> ValidatorT m ()
lengthAtMost maxLen f =
  let len = length f
   in (when (len > maxLen) $ failNonfatal $ TooLarge maxLen)

-- | Validate that a foldable's length is within the given inclusive range.
lengthBetween :: (Monad m, Foldable f) => Int -> Int -> f a -> ValidatorT m ()
lengthBetween minLen maxLen f = do
  lengthAtLeast minLen f
  lengthAtMost maxLen f

-- | Validate that a value is at least the given minimum.
valueAtLeast :: (Ord a, Typeable a, Show a, AsTranslateParam a, Monad m) => a -> a -> ValidatorT m ()
valueAtLeast minVal val =
  when (val < minVal) $ failNonfatal $ TooSmall minVal

-- | Validate that a value is at most the given maximum.
valueAtMost :: (Ord a, Typeable a, Show a, AsTranslateParam a, Monad m) => a -> a -> ValidatorT m ()
valueAtMost maxVal val =
  when (val > maxVal) $ failNonfatal $ TooLarge maxVal

-- | Validate that a value is within the given inclusive range.
valueBetween :: (Ord a, Typeable a, Show a, AsTranslateParam a, Monad m) => a -> a -> a -> ValidatorT m ()
valueBetween minVal maxVal val = do
  valueAtLeast minVal val
  valueAtMost maxVal val
