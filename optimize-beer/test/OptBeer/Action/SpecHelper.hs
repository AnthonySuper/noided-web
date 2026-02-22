{-# LANGUAGE AllowAmbiguousTypes #-}

module OptBeer.Action.SpecHelper
  ( TransactionRunner,
    runWithRunner,
    runDBSetup,
    usingTransactionRunner,
    runFailingError,
    TransactingSpec,
  )
where

import Control.Exception
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Typeable
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import GHC.Generics
import Hasql.Connection (Connection)
import Hasql.Connection qualified as C
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session (statement)
import Hasql.Statement (unpreparable)
import Noided.Sql
import Noided.Web
import Noided.Web.Internal.Effect.RunTransaction qualified as RT
import Test.Hspec

data WrappedError e = WrapError {callStack :: CallStack, wrapped :: e}
  deriving (Show, Generic)

instance (Typeable e, Show e) => Exception (WrappedError e)

-- | Run failing on error by throwing an exception wrapped in `WrappedError` in IO.
-- Most useful if you want to just fail a spec when it happens.
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

-- | Run a database transaction for setup purposes, failing on any error.
runDBSetup :: TransactionRunner -> TransactM () a -> IO a
runDBSetup runner action = runEff . runFailingError @SessionError . runFailingError @() . runWithRunner runner $ runTransaction @() action

type TransactingSpec = SpecWith TransactionRunner

-- | Use a transaction runner in each individual action.
-- Note that:
--
-- 1. A connection will be checked out from the pool for the duration of the action
-- 2. This connection will execute *within a transaction* for the duration of the action.
--    This transaction will be marked as `REPEATABLE READ`.
--    This transaction will be rolled back at the end of the example.
--    We do this by running raw commands using Hasql upon checking out a connection, and then immediately before checking it back in.
-- 3. Each transaction ran using the runner will be *shared*.
--    This means that database operations will persist from transaction to transaction.
--    That makes doing things much easier.
usingTransactionRunner :: SpecWith TransactionRunner -> SpecWith (Pool Connection)
usingTransactionRunner = aroundWith $ \action pool ->
  Pool.withResource pool $ \conn -> do
    let begin = statement () $ unpreparable "BEGIN ISOLATION LEVEL REPEATABLE READ" Enc.noParams Dec.noResult
        rollback = statement () $ unpreparable "ROLLBACK" Enc.noParams Dec.noResult

    _ <- C.use conn begin

    let runTransactionShared :: (IOE :> es) => Eff (RunTransaction : es) a -> Eff es a
        runTransactionShared = interpret $ \_ (RT.RunTransaction _ act) -> do
          liftIO $ unsafeFakeTransaction noStatementCallback act conn

    let runner = RunTransaction runTransactionShared

    action runner `finally` C.use conn rollback
