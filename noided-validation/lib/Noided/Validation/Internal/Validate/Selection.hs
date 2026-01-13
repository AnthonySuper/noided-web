module Noided.Validation.Internal.Validate.Selection where

import Data.Typeable (Typeable)
import Noided.Translate (AsTranslateParam)
import Noided.Validation.Internal.ValidationError.Selection
import Noided.Validation.Internal.Validator

-- | Validate that a value is one of the allowed options.
oneOf :: (Monad m, Eq a, Show a, Ord a, Typeable a, AsTranslateParam a) => [a] -> a -> ValidatorT m ()
oneOf options val
  | val `elem` options = return ()
  | otherwise = failNonfatal $ InvalidSelection options val

-- | Validate that a value is NOT one of the forbidden options.
noneOf :: (Monad m, Eq a, Show a, Ord a, Typeable a, AsTranslateParam a) => [a] -> a -> ValidatorT m ()
noneOf forbidden val
  | val `elem` forbidden = failNonfatal $ ForbiddenSelection forbidden val
  | otherwise = return ()
