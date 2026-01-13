module Noided.Validation.Internal.Validate.Selection where

import Noided.Validation.Internal.ValidationError.Selection
import Noided.Validation.Internal.Validator

-- | Validate that a value is one of the allowed options.
oneOf :: (Foldable f, Monad m, Eq a) => f a -> a -> ValidatorT m ()
oneOf options val
  | val `elem` options = return ()
  | otherwise = failNonfatal InvalidSelection

-- | Validate that a value is NOT one of the forbidden options.
noneOf :: (Foldable f, Monad m, Eq a) => f a -> a -> ValidatorT m ()
noneOf forbidden val
  | val `elem` forbidden = failNonfatal ForbiddenSelection
  | otherwise = return ()
