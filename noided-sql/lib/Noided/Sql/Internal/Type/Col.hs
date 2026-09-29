{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Noided.Sql.Internal.Type.Col
-- Description: Field wrapper for HKD tables.
--
-- 'Col' only computes on the static column descriptor, never on the wrapper
-- @f@. That means
-- @Col (RegularColumn Text) f@ reduces to @f ('NonNullT' Text)@ for /any/ @f@,
-- so GHC can infer @f@ from a field's value. The column's default-ness is not
-- visible after reduction; 'Noided.Sql.Internal.TH.Table.deriveTable'
-- recovers it from the declared (unreduced) field type via @reify@.
module Noided.Sql.Internal.Type.Col
  ( Col,
    Table (..),
    QueryCol (..),
  )
where

import Data.Kind (Type)
import Noided.Row
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.SqlType

type Col :: ColumnType -> (SqlType -> Type) -> Type
type family Col c f where
  Col (Column _ n t) f = f (SqlT n t)

-- | Tables defined with 'Noided.Sql.Internal.TH.Table.deriveTable'.
--
-- Carries the flattened, type-level column definitions (with defaults), which
-- are needed for INSERT / UPDATE checking.
class Table (t :: (SqlType -> Type) -> Type) where
  type TableColumns t :: [RowLabel ColumnType]

  -- | Flatten a row into its columns, in declaration order, nested tables
  -- included.
  toColumnRow :: t f -> WrappedRow (TableColumns t) (QueryCol f)

-- | A table field, indexed by its column definition rather than by the
-- 'SqlType' it has in a query.
type QueryCol :: (SqlType -> Type) -> ColumnType -> Type
newtype QueryCol f c = QueryCol {getQueryCol :: f (ColumnInQuery c)}
