module Noided.Sql.Merge
  ( MergeQuery,
    mergeReturning,
    mergeReturningAll,

    -- * Merge clauses
    MergeClause,
    whenMatched_,
    whenNotMatched_,
    whenNotMatchedBySource_,
    andMergeCondition_,

    -- * Merge actions
    MergeClauseAction (..),
    MergeWhenCondition (..),
  )
where

import Noided.Sql.Internal.Merge.Merge
