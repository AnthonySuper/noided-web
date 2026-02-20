module Noided.Sql.Define
  ( -- * Defining tables
    TableDefinition (..),
    TableName (..),
    tableNameNoSchema,

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
import Noided.Sql.Internal.TH.HKDTable
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.Columnar
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName
