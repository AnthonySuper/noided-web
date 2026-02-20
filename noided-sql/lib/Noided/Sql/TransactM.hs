-- |
-- Module: Noided.Sql.TransactM
-- Description: The 'TransactM' monad for executing SQL transactions.
--
-- This module provides the 'TransactM' monad, which is used to execute SQL queries within a transaction.
-- It supports different isolation levels, savepoints, and error handling.
module Noided.Sql.TransactM
  ( -- * The TransactM Monad
    TransactM,
    execSqlStatement,

    -- * Running Transactions
    transactSerialized,
    transactRepeatableRead,
    transactDryRun,

    -- * Transaction Results
    TransactionResult (..),
    TransactionIsolation (..),

    -- * Callbacks
    StatementCallback,
    statementCallback,
    noStatementCallback,

    -- * Savepoints
    savepoint,
    savepointNamed,
    SavepointName,

    -- * Query Helpers
    querySingleRow,
    queryMaybe,
    queryVector,
    queryFoldl,
    queryFoldr,
  )
where

import Noided.Sql.Internal.Type.TransactM
import Noided.Sql.Internal.Type.TransactionResult
