{-# LANGUAGE AllowAmbiguousTypes #-}

module OptBeer.Action.SpecHelper (TransactionRunner, runWithRunner, usingTransactionRunner, runFailingError) where

import Control.Exception
import Data.Pool (Pool)
import Data.Typeable
import Effectful
import Effectful.Error.Static
import GHC.Generics
import GHC.Stack
import Hasql.Connection (Connection)
import Noided.Web
import Test.Hspec

data WrappedError e = WrapError {callStack :: CallStack, wrapped :: e}
  deriving (Show, Generic)

instance (Typeable e, Show e) => Exception (WrappedError e)

-- | Run failing on error by throwing an exception wrapped in `WrappedError`.
runFailingError ::
  forall e es a.
  (Typeable e, Show e, IOE :> es) =>
  Eff (Error e : es) a ->
  Eff es a
runFailingError = runErrorWith $ \cs e ->
  liftIO (throwIO $ WrapError cs e)

data TransactionRunner where
  RunTransaction ::
    ( forall a es.
      (IOE :> es) =>
      Eff (RunTransaction : es) a ->
      Eff es a
    ) ->
    TransactionRunner

runWithRunner :: (IOE :> es) => TransactionRunner -> Eff (RunTransaction : es) a -> Eff es a
runWithRunner (RunTransaction e) r = e r

type TransactingSpec = SpecWith TransactionRunner

-- | Use a transaction runner in each individual action.
-- Note that:
--
-- 1. A connection will be checked out from the pool for the duration of the action
-- 2. This connection will execute *withtin a transaction* for the duration of the action.
--    This transaction will be marked as `REPEATABLE READ`.
--    This transaction will be rolled back at the end of the example.
-- 3. Each transaction ran using the runner will be *shared*.
--    This means that database operations will persist from transaction to transaction.
--    That makes doing things much easier.
usingTransactionRunner :: SpecWith TransactionRunner -> SpecWith (Pool Connection)
usingTransactionRunner = aroundWith (error "TODO: implement me")
