module Noided.Validation.Internal.Validate.Blank where

import Data.List.NonEmpty qualified as NE
import Noided.Validation.Internal.ValidationError.Blank
import Noided.Validation.Internal.Validator

-- | Attempt to validate that a list is nonempty.
-- If successful, return a 'NonEmpty list.
-- Fail fatally with 'Blank' otherwise.
listNonBlankNE :: (Monad m) => [a] -> ValidatorT m (NE.NonEmpty a)
listNonBlankNE lst =
  case NE.nonEmpty lst of
    Just ne -> pure ne
    Nothing -> failFatal Blank

-- | Validate that a foldable is non-blank.
-- Fails nonfatally so validation can continue.
foldableNonBlank :: (Monad m, Foldable f) => f a -> ValidatorT m ()
foldableNonBlank e
  | null e = failNonfatal Blank
  | otherwise = return ()
