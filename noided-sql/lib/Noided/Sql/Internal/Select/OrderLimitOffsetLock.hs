{-# LANGUAGE LambdaCase #-}

module Noided.Sql.Internal.Select.OrderLimitOffsetLock where

import Data.Bifunctor
import Data.HKD
import Data.Int (Int64)
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Select.AggregateQuery
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.Type.OrderClause
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr

data TieInclusion
  = TiesIncluded
  | TiesExcluded
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | Type of the FETCH FIRST clause.
-- We need to know if there is a clause, and if it includes ties, because
-- that will determine what locking behavior should be.
type FetchFirstClause :: Maybe TieInclusion -> Type
data FetchFirstClause ti where
  NoFetchFirstClauset :: FetchFirstClause Nothing
  FetchFirstClauseOnly :: Int64 -> FetchFirstClause (Just TiesExcluded)
  FetchFirstClauseWithTies :: Int64 -> FetchFirstClause (Just TiesIncluded)

-- | Data kind to determine if an order by clause exists.
data OrderByUsage = NoOrderBy | HasOrderBy
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

type OrderByClause :: SqlScope -> OrderByUsage -> Type
data OrderByClause scope hob where
  NoOrder :: OrderByClause anyScope NoOrderBy
  OrderQueryBy ::
    OrderClause scope ->
    OrderByClause scope HasOrderBy

data LockKind = ForUpdate | ForNoKeyUpdate | ForShare | ForKeyShare
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

data SkipLockedUsage = NoSkipLocked | HasSkipLocked
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

type LockWait :: SkipLockedUsage -> Type
data LockWait usage where
  Wait :: LockWait NoSkipLocked
  NoWait :: LockWait NoSkipLocked
  SkipLocked :: LockWait HasSkipLocked

type LockingClause :: Maybe SkipLockedUsage -> Type
data LockingClause skipLockUsage where
  NoLockingClause :: LockingClause Nothing
  LockingClause ::
    { lockKind :: LockKind,
      lockWait :: LockWait usage
    } ->
    LockingClause (Just usage)

data OrderLimitOffsetLockCfg scope tieInclusion orderByUsage skipLockedUsage
  = MkOrderLimitOffsetLockCfg
  { offsetClause :: Maybe Int64,
    fetchFirstClause :: FetchFirstClause tieInclusion,
    orderByClause :: OrderByClause scope orderByUsage,
    lockingClause :: LockingClause skipLockedUsage
  }
  deriving (Generic)

type ValidOrderLimitOffsetLock ::
  Maybe TieInclusion ->
  OrderByUsage ->
  Maybe SkipLockedUsage ->
  Constraint
type family ValidOrderLimitOffsetLock ti obu slu where
  ValidOrderLimitOffsetLock (Just TiesIncluded) NoOrderBy _ =
    TypeError (Text "Cannot use WITH TIES without an order clause")
  ValidOrderLimitOffsetLock (Just TiesIncluded) _ (Just HasSkipLocked) =
    TypeError (Text "Cannot use WITH TIES with SKIP LOCKED")
  ValidOrderLimitOffsetLock _ _ _ = ()

data OrderLimitOffsetLock expectedScope result where
  OrderLimitOffsetLockSelect ::
    (ValidOrderLimitOffsetLock ti obu slu) =>
    SelectM selectResult ->
    (selectResult -> (result, OrderLimitOffsetLockCfg NormalQuery ti obu slu)) ->
    OrderLimitOffsetLock NormalQuery result
  OrderLimitOffsetLockAggregated ::
    ( ValidOrderLimitOffsetLock ti obu slu,
      slu ~ Nothing
    ) =>
    AggregateQuery aggregatedResult ->
    (aggregatedResult -> (result, OrderLimitOffsetLockCfg Aggregated ti obu slu)) ->
    OrderLimitOffsetLock Aggregated result

instance Functor (OrderLimitOffsetLock expectedScope) where
  fmap f = \case
    OrderLimitOffsetLockSelect sl buildRes ->
      OrderLimitOffsetLockSelect sl (fmap (first f) buildRes)
    OrderLimitOffsetLockAggregated sl buildRes ->
      OrderLimitOffsetLockAggregated sl (fmap (first f) buildRes)

instance
  (SelectList sl, wrapper ~ SqlExpr someScope, someScope ~ expectedScope) =>
  Query (OrderLimitOffsetLock expectedScope (sl wrapper))
  where
  type
    QuerySelectList (OrderLimitOffsetLock expectedScope (sl wrapper)) =
      sl
  writeQuerySyntax = renderOrderLimitOffsetLock

instance
  (SelectList sl, wrapper ~ SqlExpr someScope, someScope ~ expectedScope) =>
  SelectQuery (OrderLimitOffsetLock expectedScope (sl wrapper))

instance
  ( SelectList sl,
    wrapper ~ SqlExpr someScope,
    someScope ~ expectedScope,
    UnwrapSelectList sl,
    DecodeSelectList sl
  ) =>
  ExecutableQuery (OrderLimitOffsetLock expectedScope (sl wrapper))

renderOrderLimitOffsetLock ::
  (SelectList result) =>
  OrderLimitOffsetLock expectedScope (result (SqlExpr expectedScope)) ->
  QueryWriter ()
renderOrderLimitOffsetLock ol = error "TODO: implement me"
