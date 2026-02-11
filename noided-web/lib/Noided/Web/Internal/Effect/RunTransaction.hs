{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.RunTransaction where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Hasql.Connection (Connection)
import Hasql.Errors
import Noided.Sql.Internal.Type.TransactM
import Noided.Sql.Internal.Type.TransactionResult

data RunTransaction :: Effect where
  RunTransaction :: TransactionIsolation -> TransactM e res -> RunTransaction m (TransactionResult e res)

type instance DispatchOf RunTransaction = Dynamic

runTransactionToResult' ::
  ( RunTransaction :> es
  ) =>
  TransactionIsolation ->
  TransactM e res ->
  Eff es (TransactionResult e res)
runTransactionToResult' iso = send . RunTransaction iso

runTransactionToResult :: (RunTransaction :> es) => TransactM e res -> Eff es (TransactionResult e res)
runTransactionToResult = runTransactionToResult' Serializable

runTransactionEither' ::
  ( Error SessionError :> es,
    RunTransaction :> es
  ) =>
  TransactionIsolation ->
  TransactM a b ->
  Eff es (Either a b)
runTransactionEither' iso act = do
  res <- runTransactionToResult' iso act
  case res of
    SessionErr e -> throwError e
    TransactErr e -> return (Left e)
    TransactOK r -> return (Right r)

runTransactionEither :: (Error SessionError :> es, RunTransaction :> es) => TransactM a b -> Eff es (Either a b)
runTransactionEither = runTransactionEither' Serializable

runTransaction' ::
  ( Error a :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    Show a
  ) =>
  TransactionIsolation ->
  TransactM a b ->
  Eff es b
runTransaction' iso act = runTransactionEither' iso act >>= either throwError pure

runTransaction ::
  ( Error a :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    Show a
  ) =>
  TransactM a b ->
  Eff es b
runTransaction = runTransaction' Serializable

runRunTransaction :: (IOE :> es) => StatementCallback -> Connection -> Eff (RunTransaction : es) a -> Eff es a
runRunTransaction sc conn = interpret $ \_ (RunTransaction iso act) ->
  liftIO $
    transactWithIsolation sc conn iso act
