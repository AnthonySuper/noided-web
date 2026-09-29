{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Noided.Sql.Internal.Type.PGSeries where

import GHC.TypeLits (ErrorMessage (..), TypeError)

import Data.HKD
import Data.Int (Int32, Int64)
import Data.Kind (Constraint, Type)
import Data.Scientific (Scientific)
import Data.Time (LocalTime, UTCTime)
import Noided.Sql.Internal.Type.Interval
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

-- | Haskell types that PostgreSQL's @generate_series@ can produce.
-- PostgreSQL only has @generate_series@ for @int4@, @int8@, @numeric@,
-- @timestamp@ and @timestamptz@; anything else is a custom type error.
type SeriesElement :: Type -> Constraint
type family SeriesElement a where
  SeriesElement Int32 = ()
  SeriesElement Int64 = ()
  SeriesElement Scientific = ()
  SeriesElement LocalTime = ()
  SeriesElement UTCTime = ()
  SeriesElement a = TypeError (UnsupportedSeries a)

-- | The type of the step. Integers and numerics step by themselves; timestamps step by an 'Interval'.
type SeriesStep :: Type -> Type
type family SeriesStep a where
  SeriesStep Int32 = Int32
  SeriesStep Int64 = Int64
  SeriesStep Scientific = Scientific
  SeriesStep LocalTime = Interval
  SeriesStep UTCTime = Interval
  SeriesStep a = TypeError (UnsupportedSeries a)

-- | The type of the resulting column.
type SeriesResult :: Type -> Type
type family SeriesResult a where
  SeriesResult Int32 = Int32
  SeriesResult Int64 = Int64
  SeriesResult Scientific = Scientific
  SeriesResult LocalTime = LocalTime
  SeriesResult UTCTime = UTCTime
  SeriesResult a = TypeError (UnsupportedSeries a)

type UnsupportedSeries a =
  'Text "PostgreSQL has no generate_series for " ':<>: 'ShowType a ':<>: 'Text "."
    ':$$: 'Text "Supported element types are Int32, Int64, Scientific, LocalTime and UTCTime."
    ':$$: 'Text "Hint: cast Int16 to Int32 first. For Day, cast to LocalTime (::timestamp) first."

-- | Element types for which PostgreSQL has a two-argument @generate_series@
-- (no step). Timestamps are deliberately excluded: they need 'generateSeriesStep'.
type SeriesDefaultStep :: Type -> Constraint
type family SeriesDefaultStep a where
  SeriesDefaultStep Int32 = ()
  SeriesDefaultStep Int64 = ()
  SeriesDefaultStep Scientific = ()
  SeriesDefaultStep LocalTime = TypeError (NeedsStep LocalTime)
  SeriesDefaultStep UTCTime = TypeError (NeedsStep UTCTime)
  SeriesDefaultStep a = TypeError (UnsupportedSeries a)

type NeedsStep a =
  'Text "PostgreSQL has no two-argument generate_series for " ':<>: 'ShowType a ':<>: 'Text "."
    ':$$: 'Text "Use generateSeriesStep with an Interval step instead."

-- | Maps a series element type to its step type, via 'SeriesStep'.
type StepType :: SqlType -> SqlType
type family StepType t where
  StepType (SqlT n a) = SqlT n (SeriesStep a)

-- | Represents a call to the PostgreSQL @generate_series@ set-returning function.
-- This can be used as a FROM item in a SELECT query.
--
-- Use 'generateSeries' or 'generateSeriesStep' to construct values of this type.
data PGSeries (t :: SqlType)
  = PGSeries
  { pgSeriesStart :: SqlExpr NormalQuery t
  , pgSeriesStop :: SqlExpr NormalQuery t
  , pgSeriesStep :: Maybe (SqlExpr NormalQuery (StepType t))
  }

instance (SeriesElement a) => FromItem (PGSeries (SqlT n a)) where
  type FromItemSelectList (PGSeries (SqlT n a)) = Element (SqlT n (SeriesResult a))
  fromItemAlias _ = "series"
  fromItemLateralUsage _ = SometimesLateral
  fromItemColumnAliases _ = ColumnAliases
  writeFromItem (PGSeries start stop mStep) = do
    "generate_series("
    writeSyntax (unsafeGetSqlExpr start)
    ", "
    writeSyntax (unsafeGetSqlExpr stop)
    case mStep of
      Nothing -> pure ()
      Just step -> do
        ", "
        writeSyntax (unsafeGetSqlExpr step)
    ")"
  fromItemSelectList _ = Element "generate_series"

-- | Construct a @generate_series@ FROM item with a start and stop value.
-- The step defaults to 1. Only available for @Int32@, @Int64@ and @Scientific@;
-- timestamp series must use 'generateSeriesStep'.
generateSeries ::
  (SeriesDefaultStep a) =>
  SqlExpr NormalQuery (SqlT n a) ->
  SqlExpr NormalQuery (SqlT n a) ->
  PGSeries (SqlT n a)
generateSeries start stop = PGSeries start stop Nothing

-- | Construct a @generate_series@ FROM item with a start, stop, and step value.
-- For integer and numeric types, the step has the same type as the elements.
-- For timestamp types (@UTCTime@, @LocalTime@), the step must be a 'Interval'
-- (PostgreSQL @interval@) — see 'StepType'.
generateSeriesStep ::
  (SeriesElement a) =>
  SqlExpr NormalQuery (SqlT n a) ->
  SqlExpr NormalQuery (SqlT n a) ->
  SqlExpr NormalQuery (SqlT n (SeriesStep a)) ->
  PGSeries (SqlT n a)
generateSeriesStep start stop step = PGSeries start stop (Just step)

