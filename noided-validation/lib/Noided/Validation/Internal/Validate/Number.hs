module Noided.Validation.Internal.Validate.Number where

import Noided.Validation.Internal.ValidationError.Number
import Noided.Validation.Internal.Validator

-- | Validate that an integral number is odd.
isOdd :: (Monad m, Integral a) => a -> ValidatorT m ()
isOdd val =
  if odd val
    then return ()
    else failNonfatal $ NotOdd (toInteger val)

-- | Validate that an integral number is even.
isEven :: (Monad m, Integral a) => a -> ValidatorT m ()
isEven val =
  if even val
    then return ()
    else failNonfatal $ NotEven (toInteger val)
