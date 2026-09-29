{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module: Noided.Sql
-- Description: The noided-sql library for type-safe SQL queries.
--
-- 'noided-sql' is a SQL interface for the 'noided-web' library, providing a type-safe way to define
-- tables and write SQL queries using Higher-Kinded Data (HKD).
--
-- === Getting Started
--
-- 1.  Define your tables using HKD structures and 'Noided.Sql.Define.deriveTable'.
-- 2.  Use 'Noided.Sql.Define.tableSnakeCased' to create table definitions.
-- 3.  Write queries using the 'Noided.Sql.Select', 'Noided.Sql.Insert', 'Noided.Sql.Update', and 'Noided.Sql.Delete' modules.
-- 4.  Use 'Noided.Sql.SqlExpr' to write SQL expressions within your queries.
-- 5.  Execute your queries using the 'Noided.Sql.TransactM' monad.
--
-- === Naming conventions
--
-- Every public function that builds SQL (queries, clauses, expressions, bind
-- params, set-returning functions, and so on) has a name ending in @_@, for
-- example 'Noided.Sql.Select.select_', 'Noided.Sql.Update.update_',
-- 'Noided.Sql.SqlExpr.bindParam_' and 'Noided.Sql.SqlExpr.count_'. The suffix
-- also avoids clashes with common names such as @Data.Map.update@ or
-- @Data.List.delete@. Operators, type constructors, and helpers that do not
-- build SQL are exempt. Aliases introduced for SQL that we generate (table and
-- column aliases, including mutation targets) are always quoted.
--
-- See the documentation in each module for more details and examples.
module Noided.Sql
  ( module Noided.Sql.Define,
    module Noided.Sql.TransactM,
    module Noided.Sql.Select,
    module Noided.Sql.Insert,
    module Noided.Sql.Update,
    module Noided.Sql.Delete,
    module Noided.Sql.Merge,
    module Noided.Sql.SqlExpr,
    (:-:) (..),
    (:--:) (..),
  )
where

import Noided.Sql.Define
import Noided.Sql.Delete
import Noided.Sql.Insert
import Noided.Sql.Internal.Type.Tie
import Noided.Sql.Merge
import Noided.Sql.Select
import Noided.Sql.SqlExpr
import Noided.Sql.TransactM
import Noided.Sql.Update
