{-# LANGUAGE OverloadedStrings #-}

module OptBeer.DB.Table.SpecHelper where

import Data.Pool (Pool, withResource)
import Data.Vector (Vector)
import Hasql.Connection (Connection)
import Noided.Sql
import Noided.Sql.TransactM
import Test.Hspec
import Noided.Form.HKD

-- | Asserts that a table def is valid, by ensuring it has correct columns.
-- This will use `SelectM` to try to select a value from the table with a `WHERE FALSE` where clause.
assertValidTableDef ::
  forall columnDefs sl.
  (SelectList sl, UnwrapSelectList sl, DecodeSelectList sl) =>
  TableDefinition columnDefs sl ->
  Pool Connection ->
  Expectation
assertValidTableDef td pool = withResource pool $ \conn -> do
  let selectQuery = do
        row <- addFrom_ (fromBase_ td)
        addWhere_ false_
        return row
  res <- transactDryRun noStatementCallback (queryVector selectQuery :: TransactM String (Vector (SelectListUnwrapped sl))) conn
  case res of
    TransactOK _ -> return ()
    TransactErr e -> expectationFailure $ "Transaction error (possibly schema mismatch): " <> show e
    SessionErr e -> expectationFailure $ "Session error (database connection failed?): " <> show e

type ConnectionSpec = SpecWith (Pool Connection)

-- | Run a transaction and fail the test if it fails.
-- This uses `transactDryRun`, meaning all changes will be rolled back upon completion.
runDB :: (Show e) => TransactM e a -> Pool Connection -> IO a
runDB action pool = withResource pool $ \conn -> do
  res <- transactDryRun noStatementCallback action conn
  case res of
    TransactOK a -> return a
    TransactErr e -> expectationFailure ("Transaction failed: " <> show e) >> error "test failed"
    SessionErr e -> expectationFailure ("Session failed: " <> show e) >> error "test failed"

-- | Validate a form within a database transaction.
validateFormDB :: (HKDForm subform) => FormValidator (TransactM e) (SubformField subform) -> subform FormInput -> TransactM e (Either (FormErrors (SubformField subform)) (subform FormResult))
validateFormDB = validateForm
