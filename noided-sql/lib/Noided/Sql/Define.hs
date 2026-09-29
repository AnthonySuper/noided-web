{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module: Noided.Sql.Define
-- Description: Tools for defining SQL tables and views using HKD structures.
--
-- A table is a record parameterised over a single wrapper @f@. The same type
-- is used for a row in a query (@UserF (SqlExpr scope)@), for column names
-- (@UserF ColumnName@), and so on; 'deriveTable' generates a plain Haskell
-- record for decoded rows.
--
-- === Example: Basic Table Definition
--
-- > data UserF f = UserF
-- >   { id   :: Col (IdentityColumn Int64) f,
-- >     name :: Col (RegularColumn Text) f
-- >   }
-- >   deriving (Generic)
-- >
-- > -- Generates the User record, the UserNullF copy for outer joins, and instances.
-- > $(deriveTable ''UserF)
-- >
-- > -- Define the table, with automatic snake_casing for columns.
-- > usersTable :: TableDefinition (TableColumns UserF) UserF
-- > usersTable = tableSnakeCased "users"
--
-- Use 'tableWithNames' to name the columns yourself.
--
-- === Example: Nested Tables
--
-- You can nest tables to represent logical groupings of columns:
--
-- > data ProfileF f = ProfileF
-- >   { bio :: Col (RegularColumn Text) f,
-- >     url :: Col (Column NoDefault Nullable Text) f
-- >   }
-- >   deriving (Generic)
-- > $(deriveTable ''ProfileF)
-- >
-- > data UserWithProfileF f = UserWithProfileF
-- >   { id      :: Col (IdentityColumn Int64) f,
-- >     profile :: ProfileF f
-- >   }
-- >   deriving (Generic)
-- > $(deriveTable ''UserWithProfileF)
--
-- The nested columns are flattened into the table: @"id"@, @"bio"@ and @"url"@.
--
-- === Example: View Definition
--
-- For SQL views (or any select-only result type), use 'deriveView' and
-- 'viewSnakeCased' instead. View fields have no column defaults, so they can
-- be written with bare SQL types. Note that a view type does not /have/ to
-- correspond to an actual SQL view; you can also populate the fields directly
-- as the return value of a 'SelectM'.
--
-- > data UserSummaryF f = UserSummaryF
-- >   { id   :: f (NonNullT Int64),
-- >     name :: f (NonNullT Text)
-- >   }
-- >   deriving (Generic)
-- >
-- > $(deriveView ''UserSummaryF)
-- >
-- > userSummaryView :: ViewDef UserSummaryF
-- > userSummaryView = viewSnakeCased "user_summary"
module Noided.Sql.Define
  ( -- * Defining tables
    TableDefinition (..),
    TableName (..),
    tableNameNoSchema,
    Table (..),
    Col,
    QueryCol (..),
    deriveTable,
    deriveTableWith,
    tableSnakeCased,
    tableWithNames,
    tableColumnNames,

    -- * Outer joins
    DenullRow (..),

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

    -- * Defining views
    ViewDef (..),
    deriveView,
    deriveViewWith,
    viewSnakeCased,
    viewWithNames,

    -- * Defining Column Types
    PGType (..),
    AsBindParam (..),
    EncoderOf (..),
    AsHaskellValue (..),
    decodeNewtypeWrapper,
    pgTypeNameNewtype,
    bindParamEncoderNewtype,
    inspectBindParamNewtype,

    -- * HKD re-exports
    module Data.HKD,
    WrappedRow,
  )
where

import Data.Coerce
import Data.Functor.Contravariant
import Data.HKD
import Data.Proxy
import Data.Text (Text)
import Hasql.Decoders qualified as Dec
import Noided.Row (WrappedRow)
import Noided.Sql.Internal.Class.AsBindParam
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Class.DenullRow
import Noided.Sql.Internal.Class.PGType
import Noided.Sql.Internal.TH.Table
import Noided.Sql.Internal.TableDef
import Noided.Sql.Internal.Type.Col
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName
import Noided.Sql.Internal.Type.ViewDefinition
import Noided.Sql.Internal.ViewDef

decodeNewtypeWrapper ::
  forall decoded {wrapped}.
  ( Coercible decoded wrapped,
    AsHaskellValue decoded,
    HaskellTypeOf decoded ~ decoded
  ) =>
  Proxy wrapped ->
  Dec.Value wrapped
decodeNewtypeWrapper Proxy =
  fmap coerce (decodeHaskellValue $ Proxy @decoded)

pgTypeNameNewtype ::
  forall decoded {wrapped}.
  (PGType decoded) =>
  Proxy wrapped ->
  Text
pgTypeNameNewtype Proxy =
  pgTypeName (Proxy @decoded)

bindParamEncoderNewtype ::
  forall decoded {wrapped}.
  ( AsBindParam decoded,
    BoundNullability decoded ~ NonNull,
    Coercible wrapped decoded
  ) =>
  EncoderOf NonNull wrapped
bindParamEncoderNewtype =
  case bindParamEncoder @decoded of
    EncodeNonNull v -> EncodeNonNull (contramap coerce v)

inspectBindParamNewtype ::
  forall decoded {wrapped}.
  ( AsBindParam decoded,
    Coercible wrapped decoded
  ) =>
  wrapped ->
  Text
inspectBindParamNewtype v =
  inspectBindParam (coerce v :: decoded)
