{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module: Noided.Sql.Define
-- Description: Tools for defining SQL tables and views using HKD structures.
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
--
-- === Example: View Definition
--
-- For SQL views (or any select-only result type), use 'ViewColumnar' and
-- 'defineHKDView' instead. Note that a type defined with 'ViewColumnar' does
-- not /have/ to correspond to an actual SQL view — you can also populate the
-- fields directly as the return value of a 'SelectM'.
--
-- > data UserSummaryF realm f = UserSummary
-- >   { summaryId   :: ViewColumnar (NonNullT Int64) realm f,
-- >     summaryName :: ViewColumnar (NonNullT Text)  realm f
-- >   }
-- >   deriving (Generic)
-- >
-- > $(defineHKDView ''UserSummaryF)
-- >
-- > userSummaryView :: ViewDef UserSummaryInQuery
-- > userSummaryView = hkdViewDef "user_summary"
module Noided.Sql.Define
  ( -- * Defining tables
    TableDefinition (..),
    TableName (..),
    tableNameNoSchema,
    hkdTableDef,
    HKDTableDef,

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

    -- * Defining views
    ViewDef (..),
    hkdViewDef,

    -- * HKD View Helpers
    defineHKDView,
    ViewColumnar,
    ViewColumnUsage (..),

    -- * Defining Column Types
    PGType (..),
    AsBindParam (..),
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
import Noided.Sql.Internal.Class.PGType
import Noided.Sql.Internal.HKDTableDef (HKDTableDef, hkdTableDef)
import Noided.Sql.Internal.HKDViewDef (hkdViewDef)
import Noided.Sql.Internal.TH.HKDTable
import Noided.Sql.Internal.TH.HKDView
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.Columnar
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName
import Noided.Sql.Internal.Type.ViewColumnar
import Noided.Sql.Internal.Type.ViewDefinition

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
