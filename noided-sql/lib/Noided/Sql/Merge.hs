module Noided.Sql.Merge
  ( MergeQuery,
    mergeReturning_,
    mergeReturningAll_,

    -- * Merge clauses
    MergeClause,
    whenMatched_,
    whenNotMatched_,
    whenNotMatchedBySource_,
    whenMatchedAnd_,
    whenNotMatchedAnd_,
    whenNotMatchedBySourceAnd_,

    -- * Merge actions
    MergeClauseAction (..),
    MergeWhenCondition (..),
    MergeInsertValues,
    mergeValues_,
    mergeDefaultValues_,
  )
where

import Noided.Sql.Internal.Merge.Merge
