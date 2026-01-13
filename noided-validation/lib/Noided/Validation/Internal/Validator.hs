{-# LANGUAGE DerivingVia #-}

module Noided.Validation.Internal.Validator where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.CPS
import Data.Functor.Identity
import Noided.Validation.Internal.ValidationError
import Noided.Validation.Internal.ValidationErrors

-- | A validation monad transformer supporting both fatal and non-fatal errors.
-- Non-fatal errors allow validation to continue.
-- Fatal ones do not.
newtype ValidatorT m a = ValidatorT {getValidatorT :: ExceptT ValidationErrors (WriterT ValidationErrors m) a}
  deriving (Functor, Applicative, Monad, Alternative) via (ExceptT ValidationErrors (WriterT ValidationErrors m))

instance MonadTrans ValidatorT where
  lift = ValidatorT . lift . lift

runValidatorT :: (Monad m) => ValidatorT m a -> m (Either ValidationErrors a)
runValidatorT v = do
  (res, acc) <- runWriterT (runExceptT (getValidatorT v))
  return $
    case res of
      Right good
        | nullErrors acc -> Right good
        | otherwise -> Left acc
      Left fatalError -> Left (acc <> fatalError)

-- | Non-transformer version of 'ValidatorT'.
type Validator = ValidatorT Identity

runValidator :: ValidatorT Identity a -> Either ValidationErrors a
runValidator = runIdentity . runValidatorT

-- | Fail validation, but allow further validations to continue.
failNonfatal :: (ValidationError e, Monad m) => e -> ValidatorT m ()
failNonfatal = ValidatorT . lift . tell . singletonError

-- | Fail validation immediately, not running any further validations.
failFatal :: (ValidationError e, Monad m) => e -> ValidatorT m a
failFatal = ValidatorT . throwE . singletonError

-- | Assert a condition. If it fails, record a non-fatal error and continue.
check :: (ValidationError e, Monad m) => Bool -> e -> ValidatorT m ()
check b e = unless b (failNonfatal e)

-- | Assert a condition. If it fails, raise a fatal error and stop.
require :: (ValidationError e, Monad m) => Bool -> e -> ValidatorT m ()
require b e = unless b (failFatal e)
