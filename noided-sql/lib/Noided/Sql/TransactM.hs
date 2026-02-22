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
    unsafeFakeTransaction,

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

    -- * Result Decoders
    SelectList,
    UnwrapSelectList (..),
    DecodeSelectList (..),

    -- * Re-Exports
    SessionError (..),
  )
where

import Hasql.Errors
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Type.TransactM
import Noided.Sql.Internal.Type.TransactionResult
