module Noided.Sql.Select
  ( SelectM,
    addFrom_,
    addWhere_,
    select_,

    -- *** Building from items
    FromClause,
    fromBase_,
    innerJoinLateral_,
    innerJoin_,
    on_,
    onNullable_,
    on_',
    (&),

    -- ** Set-Returning Functions
    PGSeries,
    generateSeries,
    generateSeriesStep,

    -- ** Combining Queries (Union/Intersect/Except)
    CombineType (..),
    CombinedQueries (..),

    -- *** UNION semigroups
    QueryCombineUnion,
    combiningUnion,
    QueryCombineUnionAll,
    combingingUnionAll,

    -- *** INTERSECT semigroups
    QueryCombineIntersect,
    combiningIntersect,
    QueryCombineIntersectAll,
    combiningIntersectAll,

    -- *** EXCEPT semigroups
    QueryCombineExcept,
    combiningExcept,
    QueryCombineExceptAll,
    combiningExceptAll,

    -- ** Aggregate Queries
    AggregateQuery,
    aggregate_,
    groupBy_,
    groupByHaving_,
  )
where

import Data.Function ((&))
import Noided.Sql.Internal.Select.AggregateQuery
import Noided.Sql.Internal.Select.Combine
import Noided.Sql.Internal.Select.FromClause
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.Type.PGSeries
