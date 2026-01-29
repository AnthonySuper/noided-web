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
  )
where

import Data.Function ((&))
import Noided.Sql.Internal.Select.Combine
import Noided.Sql.Internal.Select.FromClause
import Noided.Sql.Internal.Select.SelectM
