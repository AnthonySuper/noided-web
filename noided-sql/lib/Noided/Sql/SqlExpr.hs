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
  )
where

import Noided.Sql.Internal.Class.AsBindParam
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.SqlExpr.Bool
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
