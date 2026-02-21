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
  )
where

import Noided.Sql.Internal.Class.AsBindParam
import Noided.Sql.Internal.Class.SqlNumeric
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.SqlExpr.Bool
import Noided.Sql.Internal.SqlExpr.Numeric
import Noided.Sql.Internal.SqlExpr.Text
import Noided.Sql.Internal.Type.AggregateExpr
import Noided.Sql.Internal.Type.MutationExpr
import Noided.Sql.Internal.Type.MutationType
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
