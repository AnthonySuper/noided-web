module Noided.Sql.Internal.Select.AggregateQuery where

import Data.HKD
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Tie

data AggregateQuery result where
  -- | Build an aggregated query using GROUP BY
  AggregateGroupBy ::
    ( FTraversable queryResult,
      FTraversable groupByHKD
    ) =>
    SelectM (QueriedRow queryResult) ->
    -- | build the actual GROUP BY clause
    (QueriedRow queryResult -> QueriedRow groupByHKD) ->
    -- | Build a HAVING clause
    Maybe (AggregatedRow groupByHKD :--: AggregateSetRow queryResult -> SqlExpr Aggregated (SqlT n Bool)) ->
    -- | Given the group by clause /and/ the results of the query (now in an aggregate set),
    (AggregatedRow groupByHKD :--: AggregateSetRow queryResult -> groupResult) ->
    AggregateQuery groupResult
  -- | Build an aggregate query aggregating over an entire select list, without grouping.
  AggregateEntireQuery ::
    (FTraversable queryResult) =>
    SelectM (QueriedRow queryResult) ->
    (AggregateSetRow queryResult -> aggResult) ->
    AggregateQuery aggResult
