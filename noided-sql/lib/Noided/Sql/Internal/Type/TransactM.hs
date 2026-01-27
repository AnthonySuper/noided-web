{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Type.TransactM where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Function (fix)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Hasql.Connection qualified as C
import Hasql.Errors
import Hasql.Session
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Type.SqlQuery
import Noided.Sql.Internal.Type.TransactionResult

data StatementCallback where
  -- | Do something around every statement.
  AroundStatement ::
    ( forall a b m.
      (Monad m) =>
      -- \| Analog of `liftIO`
      (forall r. IO r -> m r) ->
      -- \| Actual query in some type.
      -- You can use this to inspect the query text or its binds.
      SqlQuery a ->
      -- \| Function that executes the query and returns some result
      (SqlQuery a -> m b) ->
      -- \| The actual result.
      m b
    ) ->
    StatementCallback

noStatementCallback :: StatementCallback
noStatementCallback = AroundStatement $ \_ q b -> b q

uniqueSavepointName :: Text -> Map.Map Text Int -> (Text, Map.Map Text Int)
uniqueSavepointName sv m =
  case Map.lookup sv m of
    Nothing -> (sv, Map.insert sv 1 m)
    Just n ->
      let nm = Map.insert sv (n + 1) m
       in uniqueSavepointName (sv <> pack ("_" <> show n)) nm

newtype TransactMEnv
  = TME {callback :: StatementCallback}

-- | State, used to make sure checkpoint names are unique.
type TransactMState = Map.Map Text Int

execQueryCallback :: StatementCallback -> SqlQuery b -> Session b
execQueryCallback cb query = do
  let AroundStatement f = cb
  f liftIO query sqlQueryToHasqlSession

execCommandCallback :: StatementCallback -> Text -> Session ()
execCommandCallback cb = execQueryCallback cb . unsafeQueryFromScript

-- | Execute an 'SqlQuery' within a transaction.
-- This will run the statement callback on the query.
execQueryRaw :: SqlQuery a -> TransactM err a
execQueryRaw query = UnsafeMKTransactM $ do
  env <- ask
  lift $ lift $ lift $ execQueryCallback env.callback query

-- | A transaction monad, with errors of type 'err'.
newtype TransactM err a = UnsafeMKTransactM
  { getTransactM ::
      ReaderT -- note: MonadReader intentionally not dervied as it is an implementation detail
        TransactMEnv
        ( ExceptT
            err
            (StateT TransactMState Session) -- note: MonadState intentionally not defined as it is an implementation detail
        )
        a
  }
  deriving newtype (Functor, Applicative, Monad, MonadError err)

collapseResult :: Either SessionError (Either e a) -> TransactionResult e a
collapseResult (Left s) = SessionErr s
collapseResult (Right (Left e)) = TransactErr e
collapseResult (Right (Right r)) = TransactOK r

-- | Transact using the SERIALIZABLE isolation level.
-- Failures due to serialization errors will be retried indefinitely (use a timeout if neccessary to prevent this).
-- Failures due to using 'throwError' inside the transaction will result in the entire transaction being rolled back.
--
-- Queries will be rolled back and retried when needed to
transactSerialized :: StatementCallback -> TransactM e a -> C.Connection -> IO (TransactionResult e a)
transactSerialized cb action conn = loop
  where
    loop = do
      sessRes <- C.use conn (runSerializedSession cb action)
      case sessRes of
        Left err -> if isSerializationFailure err then loop else pure $ SessionErr err
        Right res -> pure res

isSerializationFailureServer :: ServerError -> Bool
isSerializationFailureServer (ServerError sqlState _ _ _ _) = sqlState == "40001"

isSerializationFailure :: SessionError -> Bool
isSerializationFailure (ScriptSessionError _ s) = isSerializationFailureServer s
isSerializationFailure (StatementSessionError _ _ _ _ _ (ServerStatementError e)) = isSerializationFailureServer e
isSerializationFailure _ = False

runTransactMCommiting :: StatementCallback -> TransactM e a -> Session (TransactionResult e a)
runTransactMCommiting cb action = do
  res <- runTransactM action cb
  case res of
    Left e -> do
      execCommandCallback cb "ROLLBACK;"
      pure $ TransactErr e
    Right a -> do
      execCommandCallback cb "COMMIT;"
      pure $ TransactOK a

runSerializedSession :: StatementCallback -> TransactM e a -> Session (TransactionResult e a)
runSerializedSession cb action = fix $ \retry -> do
  execCommandCallback cb "BEGIN ISOLATION LEVEL serializable;"
  runTransactMCommiting cb action `catchError` \e ->
    if isSerializationFailure e
      then retry
      else throwError e

-- | Transact using the REPEATABLE READ isolation level.
-- Failures will not be retried for any reason.
-- If the transaction terminates early with 'throwError' it will be rolled back.
transactRepeatableRead :: StatementCallback -> TransactM e a -> C.Connection -> IO (TransactionResult e a)
transactRepeatableRead cb action conn = do
  sessRes <- C.use conn $ do
    execCommandCallback cb "BEGIN ISOLATION LEVEL REPEATABLE READ;"
    runTransactMCommiting cb action
  case sessRes of
    Left err -> pure $ SessionErr err
    Right res -> pure res

-- | Dry-run a transaction.
-- Uses the REPEATABLE READ isolation level, but *always rolls back* upon completion, regardless of success or not.
-- This is useful for writing tests: you can do a dry-run of an entire operation.
transactDryRun :: StatementCallback -> TransactM e a -> C.Connection -> IO (TransactionResult e a)
transactDryRun cb action conn = do
  sessRes <- C.use conn $ do
    execCommandCallback cb "BEGIN ISOLATION LEVEL REPEATABLE READ;"
    res <- runTransactM action cb
    execCommandCallback cb "ROLLBACK;"
    pure $ case res of
      Left e -> TransactErr e
      Right a -> TransactOK a
  case sessRes of
    Left err -> pure $ SessionErr err
    Right res -> pure res

-- | Helper to run the monad stack
runTransactM :: TransactM e a -> StatementCallback -> Session (Either e a)
runTransactM action cb = do
  let env = TME {callback = cb}
  evalStateT (runExceptT (runReaderT (getTransactM action) env)) mempty

-- | Convert from the underlying representation (with all the instances) to the 'TransactM' monad.
fromTransformers :: ReaderT TransactMEnv (ExceptT err (StateT TransactMState Session)) a -> TransactM err a
fromTransformers = UnsafeMKTransactM

-- | Convert from the 'TransactM' monad to the underlying representation.
toTransformers :: TransactM err a -> ReaderT TransactMEnv (ExceptT err (StateT TransactMState Session)) a
toTransformers = getTransactM

-- | Create a new savepoint with a given name.
-- The name will be made unique for this entire transaction.
--
-- If an error thrown with MonadError bubbles up past the point of execution, the savepoint will be rolled back
-- with @ ROLLBACK TO SAVEPOINT @. So this gets rolled back:
--
-- @
-- myAction = savepointNamed "foo" $
--   performUpdate
--   throwError () -- after this line is called, we will
-- @
--
-- Otherwise, it will be released (commited).
-- So, this will have the savepoint released, assuming @ performUpdate @ throws no error:
--
-- @
-- myAction = savepointNamed "foo" performUpdate
-- @
savepointNamed :: Text -> TransactM err a -> TransactM err a
savepointNamed name action = do
  uniqName <- fromTransformers $ do
    st <- lift $ lift get
    let (uName, newSt) = uniqueSavepointName name st
    lift $ lift $ put newSt
    pure uName

  execQueryRaw $ unsafeQueryFromScript $ "SAVEPOINT " <> uniqName

  catchError
    ( do
        res <- action
        execQueryRaw $ unsafeQueryFromScript $ "RELEASE SAVEPOINT " <> uniqName
        pure res
    )
    ( \e -> do
        execQueryRaw $ unsafeQueryFromScript $ "ROLLBACK TO SAVEPOINT " <> uniqName
        throwError e
    )

execSqlStatement :: SqlQuery a -> TransactM err a
execSqlStatement = execQueryRaw

querySingleRow :: (ExecutableQuery a) => a -> TransactM err (QueryResult a)
querySingleRow = execSqlStatement . sqlQuerySingleRow

queryMaybe :: (ExecutableQuery a) => a -> TransactM err (Maybe (QueryResult a))
queryMaybe = execSqlStatement . sqlQueryMaybe

queryVector :: (ExecutableQuery a) => a -> TransactM err (Vector (QueryResult a))
queryVector = execSqlStatement . sqlQueryVector

queryFoldl :: (ExecutableQuery a1) => (a2 -> QueryResult a1 -> a2) -> a2 -> a1 -> TransactM err a2
queryFoldl f acc = execSqlStatement . sqlQueryFoldl f acc

queryFoldr :: (ExecutableQuery a1) => (QueryResult a1 -> a2 -> a2) -> a2 -> a1 -> TransactM err a2
queryFoldr f acc = execSqlStatement . sqlQueryFoldr f acc

-- | Like 'savepointNamed', but anonymous.
-- This leads to slightly worse query logs.
savepoint :: TransactM err a -> TransactM err a
savepoint = savepointNamed "savepoint"
