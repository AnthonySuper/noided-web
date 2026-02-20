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
-- 1.  Define your tables using HKD structures and 'Noided.Sql.Define.defineHKDTable'.
-- 2.  Use 'Noided.Sql.Define.hkdTableDef' to create table definitions.
-- 3.  Write queries using the 'Noided.Sql.Select', 'Noided.Sql.Insert', 'Noided.Sql.Update', and 'Noided.Sql.Delete' modules.
-- 4.  Use 'Noided.Sql.SqlExpr' to write SQL expressions within your queries.
-- 5.  Execute your queries using the 'Noided.Sql.TransactM' monad.
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
  )
where

import Noided.Sql.Define
import Noided.Sql.Delete
import Noided.Sql.Insert
import Noided.Sql.Merge
import Noided.Sql.Select
import Noided.Sql.SqlExpr
import Noided.Sql.TransactM
import Noided.Sql.Update
