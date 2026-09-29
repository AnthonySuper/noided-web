{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Select.AggregateQuery where

import Control.Monad (void)
import Control.Monad.Trans.State.Strict
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.HKD
import GHC.Generics
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Noided.Sql.Internal.Type.Tie

data AggregateGrouping = EntireQuery | GroupByStatement
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

data AggregateQuery (grouping :: AggregateGrouping) result where
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
    AggregateQuery GroupByStatement groupResult
  -- | Build an aggregate query aggregating over an entire select list, without grouping.
  AggregateEntireQuery ::
    (FTraversable queryResult) =>
    SelectM (QueriedRow queryResult) ->
    (AggregateSetRow queryResult -> aggResult) ->
    AggregateQuery EntireQuery aggResult

instance Functor (AggregateQuery grouping) where
  fmap f (AggregateGroupBy q gb h p) = AggregateGroupBy q gb h (f . p)
  fmap f (AggregateEntireQuery q p) = AggregateEntireQuery q (f . p)

-- | Aggregate over the entire result set of a query.
aggregate_ ::
  (FTraversable sl) =>
  (AggregateSetRow sl -> res) ->
  SelectM (QueriedRow sl) ->
  AggregateQuery EntireQuery res
aggregate_ p q = AggregateEntireQuery q p

-- | Group a query by a set of columns.
groupBy_ ::
  (FTraversable sl, FTraversable gb) =>
  (QueriedRow sl -> QueriedRow gb) ->
  (AggregatedRow gb :--: AggregateSetRow sl -> res) ->
  SelectM (QueriedRow sl) ->
  AggregateQuery GroupByStatement res
groupBy_ gb p q = AggregateGroupBy q gb Nothing p

-- | Group a query by a set of columns, with a HAVING clause.
groupByHaving_ ::
  (FTraversable sl, FTraversable gb) =>
  (QueriedRow sl -> QueriedRow gb) ->
  (AggregatedRow gb :--: AggregateSetRow sl -> SqlExpr Aggregated (SqlT n Bool)) ->
  (AggregatedRow gb :--: AggregateSetRow sl -> res) ->
  SelectM (QueriedRow sl) ->
  AggregateQuery GroupByStatement res
groupByHaving_ gb h p q = AggregateGroupBy q gb (Just h) p

renderAggregateQuery ::
  (FZip sl, FTraversable sl, NamedColumns sl) =>
  AggregateQuery anyGrouping (sl (SqlExpr Aggregated)) ->
  QueryWriter (sl (SqlExpr Aggregated))
renderAggregateQuery aq = do
  case aq of
    AggregateGroupBy q gb h p -> do
      ((rawResult, finalState), _selectSyn) <- delayWriting $ runStateT (unsafeGetSelectM q) mempty
      let groupedHKD = gb rawResult
          aggregatedGroupedHKD = ffmap coerce groupedHKD
          aggregateSetRow = ffmap coerce rawResult
          combined = aggregatedGroupedHKD :--: aggregateSetRow
          result = p combined

      writeSyntax "SELECT"
      for_ (aliasedColumnList result) $ \res -> do
        writeSyntax " "
        writeSyntax res

      for_ (fromCommaSepWritten $ foldMap Written finalState.fromSyntaxes) $ \syns -> do
        writeSyntax " FROM "
        writeSyntax syns

      for_ (writeAnds finalState.whereSyntaxes) $ \act -> do
        writeSyntax " WHERE "
        act

      writeSyntax " GROUP BY "
      let gbSyns = ffoldMap (Written . unsafeGetSqlExpr) groupedHKD
      for_ (fromCommaSepWritten gbSyns) writeSyntax

      for_ h $ \havingF -> do
        writeSyntax " HAVING "
        writeSyntax (unsafeGetSqlExpr $ havingF combined)

      return result
    AggregateEntireQuery q p -> do
      ((rawResult, finalState), _selectSyn) <- delayWriting $ runStateT (unsafeGetSelectM q) mempty
      let aggregateSetRow = ffmap coerce rawResult
          result = p aggregateSetRow

      writeSyntax "SELECT"
      for_ (aliasedColumnList result) $ \res -> do
        writeSyntax " "
        writeSyntax res

      for_ (fromCommaSepWritten $ foldMap Written finalState.fromSyntaxes) $ \syns -> do
        writeSyntax " FROM "
        writeSyntax syns

      for_ (writeAnds finalState.whereSyntaxes) $ \act -> do
        writeSyntax " WHERE "
        act

      return result

instance
  (wrapper ~ SqlExpr Aggregated, SelectList sl) =>
  Query (AggregateQuery anyGrouping (sl wrapper))
  where
  type QuerySelectList (AggregateQuery anyGrouping (sl wrapper)) = sl
  writeQuerySyntax = void . renderAggregateQuery

instance
  (wrapper ~ SqlExpr Aggregated, SelectList sl) =>
  SelectQuery (AggregateQuery anyGrouping (sl wrapper))

instance
  ( wrapper ~ SqlExpr Aggregated,
    SelectList sl,
    UnwrapSelectList sl,
    DecodeSelectList sl
  ) =>
  ExecutableQuery (AggregateQuery anyGrouping (sl wrapper))

instance
  (wrapper ~ SqlExpr Aggregated, SelectList sl) =>
  FromItem (AggregateQuery anyGrouping (sl wrapper))
  where
  type FromItemSelectList (AggregateQuery anyGrouping (sl wrapper)) = sl
  fromItemLateralUsage _ = SometimesLateral
  writeFromItem a = do
    writeSyntax "("
    void $ renderAggregateQuery a
    writeSyntax ")"
  fromItemAlias _ = "subq_agg"
  fromItemSelectList _ = namedColumns
