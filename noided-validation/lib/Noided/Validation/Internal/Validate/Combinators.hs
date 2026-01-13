module Noided.Validation.Internal.Validate.Combinators where

import Noided.Validation.Internal.Validator

-- | Run a validator on a value if it is present (Just).
-- If Nothing, validation succeeds.
optional :: (Monad m) => (a -> ValidatorT m ()) -> Maybe a -> ValidatorT m ()
optional _ Nothing = return ()
optional v (Just x) = v x

-- | Run a validator only if the condition is True.
validateIf :: (Monad m) => Bool -> ValidatorT m () -> ValidatorT m ()
validateIf True v = v
validateIf False _ = return ()
