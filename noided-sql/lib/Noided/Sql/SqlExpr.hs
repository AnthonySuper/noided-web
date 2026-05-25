{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.SqlExpr
  ( -- * Data types
    SqlExpr,

    -- ** Scope
    SqlScope (..),

    -- *** Row Aliases
    QueriedRow,
    AggregateSetRow,
    AggregatedRow,

    -- ** Value aliases
    QueriedExpr,
    AggregateSetExpr,
    AggregatedExpr,

    -- *** Element Aliases

    -- ** SQL Types
    SqlType (..),

    -- *** Helper aliases
    NullableT,
    NonNullT,

    -- ** Nullability
    Nullability (..),
    CastNullability,

    -- ** Mutation expressions
    MutationExpr,
    MutationType (..),
    mutateVal_,
    mutateBound_,
    defaultVal_,

    -- * Bind params
    AsBindParam (BoundNullability, BoundType),
    bindParam,

    -- * Boolean functions
    true_,
    false_,
    (&&.),
    (||.),
    (==.),
    (/=.),
    (<.),
    (>.),
    (<=.),
    (>=.),

    -- * Nullability functions
    isNull_,
    isNotNull_,
    coalesce_,

    -- * Conditional functions
    CaseBranch,
    then_,
    case_,
    caseNoElse_,
    caseSimple_,

    -- * Working with subqueries
    aggregatedSubqueryVal_,
    subqueryRowsOf_,

    -- * Numeric functions
    SqlNumeric (SumType, AvgType),
    (+.),
    (-.),
    (*.),
    (/.),
    abs_,
    negate_,

    -- * Text functions
    (<>.),
    lower_,
    upper_,
    length_,
    trim_,
    ltrim_,
    rtrim_,
    substr_,
    replace_,

    -- * Full-text search functions
    PGTSVector,
    PGTSQuery,
    PGRegConfig,
    PGFullTextSearchWeight (..),
    (@@.),
    concatTSVector_,
    tsAnd_,
    tsOr_,
    tsNot_,
    toTSVector_,
    toTSVectorWithConfig_,
    toTSQuery_,
    toTSQueryWithConfig_,
    plainToTSQuery_,
    plainToTSQueryWithConfig_,
    phraseToTSQuery_,
    phraseToTSQueryWithConfig_,
    websearchToTSQuery_,
    websearchToTSQueryWithConfig_,
    setWeight_,
    tsRank_,
    tsRankCd_,

    -- * Inet functions
    (<<.),
    (<<=.),
    (>>.),
    (>>=.),
    inetOverlaps_,
    abbrev_,
    broadcast_,
    family_,
    host_,
    hostmask_,
    inetMerge_,
    inetSameFamily_,
    masklen_,
    netmask_,
    network_,
    setMasklen_,
    inetToText_,
    inetAddOffset_,
    inetSubOffset_,
    inetSubInet_,

    -- * Range functions
    contains_,
    isContainedBy_,

    -- * Date / time functions

    -- ** Field enumerations
    DatePartField (..),
    DateTruncField (..),

    -- ** Current date and time
    now_,
    currentDate_,
    currentTimestamp_,
    localTimestamp_,
    clockTimestamp_,
    statementTimestamp_,
    transactionTimestamp_,

    -- ** Date arithmetic
    dateAddDays_,
    dateSubDays_,
    dateDiffDays_,
    dateAddInterval_,

    -- ** Interval arithmetic
    intervalAdd_,
    intervalSub_,
    intervalNegate_,

    -- ** Timestamp arithmetic
    timestampAddInterval_,
    timestampSubInterval_,
    timestampDiff_,
    timestamptzAddInterval_,
    timestamptzSubInterval_,
    timestamptzDiff_,

    -- ** Extraction and truncation
    datePart_,
    dateTrunc_,
    dateTruncTz_,
    dateTruncInterval_,

    -- ** Age
    age_,
    ageTz_,

    -- ** Interval adjustment
    justifyDays_,
    justifyHours_,
    justifyInterval_,

    -- ** Conversion
    toTimestamp_,

    -- ** Finiteness checks
    isFiniteDate_,
    isFiniteTimestamp_,
    isFiniteTimestamptz_,
    isFiniteInterval_,

    -- * Aggregate functions
    AggregateExpr,
    agg,
    filterWhere_,
    countAll_,
    count_,
    countDistinct_,
    min_,
    max_,
    sum_,
    avg_,
    every_,
    any_,
    boolAnd_,
    boolOr_,
    stddev_,
    variance_,
    arrayAgg_,

    -- * Order clauses
    OrderClause,
    asc_,
    desc_,
    ascNullsFirst_,
    descNullsFirst_,
    ascNullsLast_,
    descNullsLast_,

    -- ** Building clauses
    NullsOrdering (..),
    SqlOrderDirection (..),
    SqlOrder (..),
    useOrdering_,
    withOrdering_,
  )
where

import Noided.Sql.Internal.Class.AsBindParam
import Noided.Sql.Internal.Class.SqlNumeric
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.SqlExpr.Bool
import Noided.Sql.Internal.SqlExpr.Case
import Noided.Sql.Internal.SqlExpr.DateTime
import Noided.Sql.Internal.SqlExpr.FullText
import Noided.Sql.Internal.SqlExpr.Inet
import Noided.Sql.Internal.SqlExpr.Numeric
import Noided.Sql.Internal.SqlExpr.Range
import Noided.Sql.Internal.SqlExpr.SubAgg
import Noided.Sql.Internal.SqlExpr.Text
import Noided.Sql.Internal.Type.AggregateExpr
import Noided.Sql.Internal.Type.CaseBranch
import Noided.Sql.Internal.Type.MutationExpr
import Noided.Sql.Internal.Type.MutationType
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.OrderClause
import Noided.Sql.Internal.Type.PGFullTextSearchWeight
import Noided.Sql.Internal.Type.PGRegConfig
import Noided.Sql.Internal.Type.PGTSQuery
import Noided.Sql.Internal.Type.PGTSVector
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

mutateBound_ :: (AsBindParam a) => a -> MutationExpr (ActualValue (SqlT (BoundNullability a) (BoundType a)))
mutateBound_ = mutateVal_ . bindParam
