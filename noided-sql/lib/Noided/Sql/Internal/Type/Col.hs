{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Noided.Sql.Internal.Type.Col
-- Description: Field wrapper for plain (realm-free) HKD tables.
--
-- Plain tables have kind @'NullTag' -> ('SqlType' -> Type) -> Type@:
--
-- > data PostF nt f = PostF
-- >   { id :: Col (IdentityColumn Int64) nt f,
-- >     title :: Col (RegularColumn Text) nt f,
-- >     author :: UserF nt f
-- >   }
--
-- The tag says whether the row came from the nullable side of an outer join.
-- @PostF 'NotNulled@ has the declared column types; @PostF 'Nulled@ has every
-- column nullable.
--
-- 'Col' has a single equation that only inspects the column descriptor, so
-- @Col c nt f@ always reduces to @f (SqlT (ApplyTag nt n) t)@, even when @nt@
-- and @f@ are unknown. That keeps @f@ inferable from a field's value. The
-- column's default-ness is not visible after reduction;
-- 'Noided.Sql.Internal.TH.PlainTable.defineTable' recovers it from the declared
-- (unreduced) field type via @reify@.
module Noided.Sql.Internal.Type.Col
  ( NullTag (..),
    ApplyTag,
    Col,
    PlainTable (..),
  )
where

import Data.Kind (Type)
import Noided.Row
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType

-- | Whether a table row comes from the nullable side of an outer join.
data NullTag = NotNulled | Nulled

type ApplyTag :: NullTag -> Nullability -> Nullability
type family ApplyTag nt n where
  ApplyTag Nulled _ = Nullable
  ApplyTag NotNulled n = n

type Col :: ColumnType -> NullTag -> (SqlType -> Type) -> Type
type family Col c nt f where
  Col (Column _ n t) nt f = f (SqlT (ApplyTag nt n) t)

-- | Tables defined with 'Noided.Sql.Internal.TH.PlainTable.defineTable'.
--
-- Carries the flattened, type-level column definitions (with defaults), which
-- are needed for INSERT / UPDATE checking.
class PlainTable (t :: NullTag -> (SqlType -> Type) -> Type) where
  type TableColumns t :: [RowLabel ColumnType]

  -- | Actual (snake_cased) column names, in declaration order.
  plainColumnNames :: WrappedRow (TableColumns t) ColumnName
