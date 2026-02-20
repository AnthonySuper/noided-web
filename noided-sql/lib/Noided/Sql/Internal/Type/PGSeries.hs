{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.PGSeries where

import Data.HKD
import Data.Time (DiffTime, LocalTime, UTCTime)
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

-- | Maps a series element type to its step type.
-- For most types, the step has the same type as the elements.
-- For timestamp types (@UTCTime@, @LocalTime@), the step is an @interval@
-- (represented as 'DiffTime'), matching PostgreSQL's @generate_series@ signature.
type StepType :: SqlType -> SqlType
type family StepType t where
  StepType (SqlT n UTCTime) = SqlT n DiffTime
  StepType (SqlT n LocalTime) = SqlT n DiffTime
  StepType t = t

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

instance FromItem (PGSeries t) where
  type FromItemSelectList (PGSeries t) = Element t
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
-- The step defaults to 1.
generateSeries ::
  SqlExpr NormalQuery t ->
  SqlExpr NormalQuery t ->
  PGSeries t
generateSeries start stop = PGSeries start stop Nothing

-- | Construct a @generate_series@ FROM item with a start, stop, and step value.
-- For integer and numeric types, the step has the same type as the elements.
-- For timestamp types (@UTCTime@, @LocalTime@), the step must be a 'DiffTime'
-- (PostgreSQL @interval@) — see 'StepType'.
generateSeriesStep ::
  SqlExpr NormalQuery t ->
  SqlExpr NormalQuery t ->
  SqlExpr NormalQuery (StepType t) ->
  PGSeries t
generateSeriesStep start stop step = PGSeries start stop (Just step)

