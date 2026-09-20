module Noided.Validation.Internal.Validator where

import Control.Monad
import Control.Monad.Morph
import Data.Functor.Identity
import Data.These
import Noided.Validation.Internal.ValidationError
import Noided.Validation.Internal.ValidationErrors

-- | A validation pass over some input, running in the base monad @m@ so that
-- checks may consult the database or anything else they need.
--
-- A validator either produces a value or fails, and it distinguishes two kinds
-- of failure:
--
-- * A /non-fatal/ one ('failNonfatal', 'check') is recorded and validation
--   carries on, so that one run can report everything wrong with the input
--   rather than only the first problem.
--
-- * A /fatal/ one ('failFatal', 'require') abandons the rest of the pass. This
--   is for input nothing further can be said about — a field that could not be
--   parsed at all, say, where every later check would be noise.
--
-- Run one with 'runValidatorT' for a straight pass-or-fail answer, or with
-- 'runValidatorTThese' to also see the non-fatal errors recorded alongside a
-- value that was nonetheless produced. 'Validator' is the usual case of a
-- validator that needs no effects at all.
--
-- The base monad can be changed after the fact with 'hoist', which is what
-- lets a validator written against a narrow monad run inside a wider one.
--
-- Accumulating errors is free of space leaks, so a pass may be arbitrarily
-- long.
newtype ValidatorT m a = ValidatorT
  { -- | Continue a validation pass on top of the non-fatal errors gathered so
    -- far. Callers normally want 'runValidatorT' or 'runValidatorTThese'.
    runValidatorTWith :: ValidationErrors -> m (Either ValidationErrors a, ValidationErrors)
  }

instance (Functor m) => Functor (ValidatorT m) where
  fmap f (ValidatorT k) = ValidatorT $ \acc ->
    (\(res, acc') -> (fmap f res, acc')) <$> k acc

instance (Monad m) => Applicative (ValidatorT m) where
  pure a = ValidatorT $ \acc -> return (Right a, acc)
  ValidatorT kf <*> ValidatorT ka = ValidatorT $ \acc -> do
    (resF, accF) <- kf acc
    case resF of
      Left bad -> return (Left bad, accF)
      Right f -> do
        (resA, accA) <- ka accF
        return (fmap f resA, accA)

instance (Monad m) => Monad (ValidatorT m) where
  ValidatorT k >>= f = ValidatorT $ \acc -> do
    (res, acc') <- k acc
    case res of
      Left bad -> return (Left bad, acc')
      Right good -> runValidatorTWith (f good) acc'

instance MonadTrans ValidatorT where
  lift m = ValidatorT $ \acc -> (\a -> (Right a, acc)) <$> m

instance MFunctor ValidatorT where
  hoist f (ValidatorT k) = ValidatorT (f . k)

-- | Run a validation pass, reporting non-fatal errors even when it still
-- managed to produce a value: 'This' errors if it failed, 'That' a value if it
-- passed cleanly, and 'These' both if it produced a value but had complaints
-- along the way.
runValidatorTThese :: (Monad m) => ValidatorT m a -> m (These ValidationErrors a)
runValidatorTThese t = do
  (res, acc) <- runValidatorTWith t mempty
  return $
    case res of
      Right good
        | nullErrors acc -> That good
        | otherwise -> These acc good
      Left bad -> This (acc <> bad)

-- | Run a validation pass, treating any error at all as a failure.
runValidatorT :: (Monad m) => ValidatorT m a -> m (Either ValidationErrors a)
runValidatorT v = do
  res <- runValidatorTThese v
  return $
    case res of
      This bad -> Left bad
      That good -> Right good
      These bad _ -> Left bad

-- | Non-transformer version of 'ValidatorT'.
type Validator = ValidatorT Identity

runValidator :: ValidatorT Identity a -> Either ValidationErrors a
runValidator = runIdentity . runValidatorT

-- | Record a set of non-fatal errors and carry on.
tellErrors :: (Monad m) => ValidationErrors -> ValidatorT m ()
tellErrors new = ValidatorT $ \acc ->
  let !acc' = acc <> new
   in return (Right (), acc')

-- | Fail validation, but allow further validations to continue.
failNonfatal :: (ValidationError e, Monad m) => e -> ValidatorT m ()
failNonfatal = tellErrors . singletonError

-- | Fail validation immediately, not running any further validations.
failFatal :: (ValidationError e, Monad m) => e -> ValidatorT m a
failFatal = failFatalMany . singletonError

-- | Fail validation immediately with a whole set of errors at once.
failFatalMany :: (Monad m) => ValidationErrors -> ValidatorT m a
failFatalMany bad = ValidatorT $ \acc -> return (Left bad, acc)

-- | Assert a condition. If it fails, record a non-fatal error and continue.
check :: (ValidationError e, Monad m) => Bool -> e -> ValidatorT m ()
check b e = unless b (failNonfatal e)

-- | Assert a condition. If it fails, raise a fatal error and stop.
require :: (ValidationError e, Monad m) => Bool -> e -> ValidatorT m ()
require b e = unless b (failFatal e)
