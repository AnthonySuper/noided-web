{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module: Noided.Sql.Define
-- Description: Tools for defining SQL tables using HKD structures.
--
-- This module provides the tools to define your SQL tables using Higher-Kinded Data (HKD) structures.
-- Using HKDs allows you to use the same data type for:
--
-- 1.  Defining the table's structure.
-- 2.  Representing a row in a query (with 'SqlExpr's).
-- 3.  Representing a row in Haskell (with actual Haskell types).
--
-- === Example: Basic Table Definition
--
-- > data UserF realm f = User
-- >   { userId   :: Columnar (Column AlwaysDefault NonNull Int64) realm f,
-- >     userName :: Columnar (Column NoDefault     NonNull Text)  realm f
-- >   }
-- >   deriving (Generic)
-- >
-- > -- This generates UserTableDef, UserInQuery, User (Haskell record), etc.
-- > $(defineHKDTable ''UserF)
-- >
-- > -- Define the table, with automatic snake_casing for columns.
-- > usersTable :: TableDefinition (HKDRowLabels UserTableDef) UserInQuery
-- > usersTable = hkdTableDef "users"
--
-- === Example: Nested HKDs
--
-- You can nest HKD structures to represent logical groupings of columns:
--
-- > data ProfileF realm f = Profile
-- >   { profileBio :: Columnar (Column NoDefault NonNull Text) realm f,
-- >     profileUrl :: Columnar (Column NoDefault Nullable Text) realm f
-- >   }
-- >   deriving (Generic)
-- > $(defineHKDTable ''ProfileF)
-- >
-- > data UserWithProfileF realm f = UserWithProfile
-- >   { userId      :: Columnar (Column AlwaysDefault NonNull Int64) realm f,
-- >     userProfile :: ProfileF realm f
-- >   }
-- >   deriving (Generic)
-- > $(defineHKDTable ''UserWithProfileF)
-- >
-- > usersWithProfilesTable :: TableDefinition (HKDRowLabels UserWithProfileTableDef) UserWithProfileInQuery
-- > usersWithProfilesTable = hkdTableDef "users"
--
-- This will result in columns named @"user_id"@, @"profile_bio"@, and @"profile_url"@ in the @"users"@ table.
module Noided.Sql.Define
  ( -- * Defining tables
    TableDefinition (..),
    TableName (..),
    tableNameNoSchema,
    hkdTableDef,

    -- * HKD Table Helpers
    defineHKDTable,
    Columnar,
    ColumnUsage (..),

    -- * Column Types
    ColumnType (..),
    ColumnDefault (..),
    IdentityColumn,
    RegularColumn,

    -- * SQL Types
    SqlType (..),
    Nullability (..),
    NullableT,
    NonNullT,

    -- * HKD re-exports
    module Data.HKD,
    WrappedRow,
  )
where

import Data.HKD
import Noided.Row (WrappedRow)
import Noided.Sql.Internal.HKDTableDef (hkdTableDef)
import Noided.Sql.Internal.TH.HKDTable
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.Columnar
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName
