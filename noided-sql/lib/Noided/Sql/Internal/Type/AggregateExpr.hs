{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Type.AggregateExpr where

import Data.Int
import Data.Kind
import Noided.Sql.Internal.Class.SqlNumeric
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

data AggregateFilter scope where
  NoAggregateFilter :: AggregateFilter scope
  AggregateFilterWhere ::
    forall n scope.
    SqlExpr scope (SqlT n Bool) ->
    AggregateFilter scope

type AggregateExpr :: SqlScope -> SqlType -> Type
data AggregateExpr scope dt
  = UnsafeMkAggExpr
  { aggExprSyntax :: Syntax,
    aggExprWhere :: AggregateFilter scope
  }

filterWhere_ ::
  AggregateExpr scope res ->
  SqlExpr scope (SqlT n Bool) ->
  AggregateExpr scope res
filterWhere_ (UnsafeMkAggExpr expr _) fr =
  UnsafeMkAggExpr {aggExprSyntax = expr, aggExprWhere = AggregateFilterWhere fr}

-- | Convert an aggregate function over a grouping set to a normal function.
agg :: AggregateExpr AggregateSet dt -> SqlExpr Aggregated dt
agg = UnsafeMkSqlExpr . unsafeAggToSyntax

-- | The expression @ COUNT(*) @.
countAll_ :: AggregateExpr scope (NonNullT Int64)
countAll_ = UnsafeMkAggExpr "COUNT(*)" NoAggregateFilter

unsafeAggToSyntax :: AggregateExpr scope dt -> Syntax
unsafeAggToSyntax (UnsafeMkAggExpr sn fr) =
  sn <> case fr of
    NoAggregateFilter -> mempty
    AggregateFilterWhere (UnsafeMkSqlExpr expr) ->
      "FILTER (WHERE " <> expr <> ")"

unsafeBuildSingleArgAgg ::
  forall {valScope} inputDt resDt.
  Syntax ->
  SqlExpr valScope inputDt ->
  AggregateExpr valScope resDt
unsafeBuildSingleArgAgg syn (UnsafeMkSqlExpr re) =
  UnsafeMkAggExpr {aggExprSyntax = syn <> "(" <> re <> ")", aggExprWhere = NoAggregateFilter}

-- | Sql @COUNT@ aggregate function.
count_ :: SqlExpr valScope w -> AggregateExpr valScope (NonNullT Int64)
count_ = unsafeBuildSingleArgAgg @_ @(NonNullT Int64) "COUNT"

-- | Sql @COUNT(DISTINCT ...)@ aggregate function.
countDistinct_ :: SqlExpr valScope (SqlT n r) -> AggregateExpr valScope (NonNullT Int64)
countDistinct_ (UnsafeMkSqlExpr re) =
  UnsafeMkAggExpr {aggExprSyntax = "COUNT(DISTINCT " <> re <> ")", aggExprWhere = NoAggregateFilter}

-- | Sql @MIN@ aggregate function.
min_ :: SqlExpr valScope (SqlT n r) -> AggregateExpr valScope (NullableT r)
min_ = unsafeBuildSingleArgAgg "MIN"

-- | Sql @MAX@ aggregate function.
max_ :: SqlExpr valScope (SqlT n r) -> AggregateExpr valScope (NullableT r)
max_ = unsafeBuildSingleArgAgg "MAX"

-- | Sql @SUM@ aggregate function.
sum_ :: (SqlNumeric r) => SqlExpr valScope (SqlT n r) -> AggregateExpr valScope (NullableT (SumType r))
sum_ = unsafeBuildSingleArgAgg "SUM"

-- | Sql @AVG@ aggregate function.
avg_ :: (SqlNumeric r) => SqlExpr valScope (SqlT n r) -> AggregateExpr valScope (NullableT (AvgType r))
avg_ = unsafeBuildSingleArgAgg "AVG"

-- | Sql @EVERY@ aggregate function (also known as @BOOL_AND@).
every_ :: SqlExpr valScope (SqlT n Bool) -> AggregateExpr valScope (NullableT Bool)
every_ = unsafeBuildSingleArgAgg "EVERY"

-- | Sql @ANY@ aggregate function (also known as @BOOL_OR@).
any_ :: SqlExpr valScope (SqlT n Bool) -> AggregateExpr valScope (NullableT Bool)
any_ = unsafeBuildSingleArgAgg "ANY"

-- | Sql @BOOL_AND@ aggregate function.
boolAnd_ :: SqlExpr valScope (SqlT n Bool) -> AggregateExpr valScope (NullableT Bool)
boolAnd_ = unsafeBuildSingleArgAgg "BOOL_AND"

-- | Sql @BOOL_OR@ aggregate function.
boolOr_ :: SqlExpr valScope (SqlT n Bool) -> AggregateExpr valScope (NullableT Bool)
boolOr_ = unsafeBuildSingleArgAgg "BOOL_OR"

-- | Sql @STDDEV@ aggregate function.
stddev_ :: (SqlNumeric r) => SqlExpr valScope (SqlT n r) -> AggregateExpr valScope (NullableT Double)
stddev_ = unsafeBuildSingleArgAgg "STDDEV"

-- | Sql @VARIANCE@ aggregate function.
variance_ :: (SqlNumeric r) => SqlExpr valScope (SqlT n r) -> AggregateExpr valScope (NullableT Double)
variance_ = unsafeBuildSingleArgAgg "VARIANCE"
