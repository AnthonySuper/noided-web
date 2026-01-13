module Noided.Validation.Internal.Validate.Size where

import Noided.Validation.Internal.ValidationError.Size
import Noided.Validation.Internal.Validator

-- | Validate that a foldable has at least the given number of elements.
lengthAtLeast :: (Monad m, Foldable f) => Int -> f a -> ValidatorT m ()
lengthAtLeast minLen f =
  let len = length f
   in if len < minLen
        then failNonfatal $ TooSmall (toInteger minLen) (toInteger len)
        else return ()

-- | Validate that a foldable has at most the given number of elements.
lengthAtMost :: (Monad m, Foldable f) => Int -> f a -> ValidatorT m ()
lengthAtMost maxLen f =
  let len = length f
   in if len > maxLen
        then failNonfatal $ TooLarge (toInteger maxLen) (toInteger len)
        else return ()

-- | Validate that a foldable's length is within the given inclusive range.
lengthBetween :: (Monad m, Foldable f) => Int -> Int -> f a -> ValidatorT m ()
lengthBetween minLen maxLen f = do
  lengthAtLeast minLen f
  lengthAtMost maxLen f

-- | Validate that a value is at least the given minimum.
valueAtLeast :: (Monad m, Integral a) => a -> a -> ValidatorT m ()
valueAtLeast minVal val =
  if val < minVal
    then failNonfatal $ TooSmall (toInteger minVal) (toInteger val)
    else return ()

-- | Validate that a value is at most the given maximum.
valueAtMost :: (Monad m, Integral a) => a -> a -> ValidatorT m ()
valueAtMost maxVal val =
  if val > maxVal
    then failNonfatal $ TooLarge (toInteger maxVal) (toInteger val)
    else return ()

-- | Validate that a value is within the given inclusive range.
valueBetween :: (Monad m, Integral a) => a -> a -> a -> ValidatorT m ()
valueBetween minVal maxVal val = do
  valueAtLeast minVal val
  valueAtMost maxVal val
