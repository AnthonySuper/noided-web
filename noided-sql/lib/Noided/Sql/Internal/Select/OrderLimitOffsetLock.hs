{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Select.OrderLimitOffsetLock where

import Control.Monad.Trans.State.Strict (runStateT)
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.HKD
import Data.Int (Int64)
import Data.Kind
import Data.String (IsString (..))
import GHC.Generics
import GHC.TypeLits
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.NamedColumns (aliasedColumnList)
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Select.AggregateQuery
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.Type.OrderClause
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.Syntax (CommaSepSyntax (..), Syntax (..), fromCommaSepWritten)
import Noided.Sql.Internal.Type.Tie ((:--:) (..))

data TieInclusion
  = TiesIncluded
  | TiesExcluded
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | Type of the FETCH FIRST clause.
-- We need to know if there is a clause, and if it includes ties, because
-- that will determine what locking behavior should be.
type FetchFirstClause :: Maybe TieInclusion -> Type
data FetchFirstClause ti where
  NoFetchFirstClause :: FetchFirstClause Nothing
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
  -- | Do not use locks with this query.
  NoLockingClause :: LockingClause Nothing
  -- | Acquire a lock with this query.
  -- Note that if the query contains a FROM item that is a subquery which
  -- performs an aggregate function, this can result in /runtime errors/.
  -- The type machinery required to prevent this at the compiler level would have made the ergonomics
  -- of this library absolutely unbearable, so we decided to not implement them.
  --
  --  Generally you really should use a higher isolation level (like @SERIALIZABLE@) for anything remotely
  --  complex. Use a locking clause only when absolutely required.
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

-- | Type family that ensures the components of the
-- @OFFSET@, @FETCH FIRST@, @ORDER BY@, and @FOR UPDATE@ components do not conflict.
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

-- | Queries that have a @OFFSET@, @FETCH FIRST@, @ORDER BY@,
-- or @FOR UPDATE@ clause applied to them.
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

offsetOrderLimitLock_ ::
  (ValidOrderLimitOffsetLock ti obu slu) =>
  Maybe Int64 ->
  (t -> OrderByClause NormalQuery obu) ->
  FetchFirstClause ti ->
  LockingClause slu ->
  SelectM t ->
  OrderLimitOffsetLock NormalQuery t
offsetOrderLimitLock_ offset buildOrder limit lock query =
  OrderLimitOffsetLockSelect query $ \r ->
    (r, MkOrderLimitOffsetLockCfg {offsetClause = offset, fetchFirstClause = limit, orderByClause = buildOrder r, lockingClause = lock})

orderLimitLock_ ::
  (ValidOrderLimitOffsetLock ti obu slu) =>
  (t -> OrderByClause NormalQuery obu) ->
  FetchFirstClause ti ->
  LockingClause slu ->
  SelectM t ->
  OrderLimitOffsetLock NormalQuery t
orderLimitLock_ = offsetOrderLimitLock_ Nothing

limitLock_ ::
  (ValidOrderLimitOffsetLock ti NoOrderBy slu) =>
  FetchFirstClause ti ->
  LockingClause slu ->
  SelectM t ->
  OrderLimitOffsetLock NormalQuery t
limitLock_ = orderLimitLock_ (const NoOrder)

offsetLimitLock_ ::
  (ValidOrderLimitOffsetLock ti NoOrderBy slu) =>
  Maybe Int64 ->
  FetchFirstClause ti ->
  LockingClause slu ->
  SelectM t ->
  OrderLimitOffsetLock NormalQuery t
offsetLimitLock_ os = offsetOrderLimitLock_ os (const NoOrder)

offsetLock_ ::
  (ValidOrderLimitOffsetLock Nothing NoOrderBy slu) =>
  Maybe Int64 ->
  LockingClause slu ->
  SelectM t ->
  OrderLimitOffsetLock NormalQuery t
offsetLock_ os = offsetLimitLock_ os NoFetchFirstClause

-- | Class for queries that can have @OFFSET@, @FETCH FIRST@, and
-- @ORDER BY@ applied to them.
class OrderOffsetLimitQuery query where
  type OrderOffsetLimitScope query :: SqlScope
  offsetOrderLimit_ ::
    (ValidOrderLimitOffsetLock ff obu Nothing) =>
    Maybe Int64 ->
    (t -> OrderByClause (OrderOffsetLimitScope query) obu) ->
    FetchFirstClause ff ->
    query t ->
    OrderLimitOffsetLock (OrderOffsetLimitScope query) t

instance OrderOffsetLimitQuery SelectM where
  type OrderOffsetLimitScope SelectM = NormalQuery
  offsetOrderLimit_ off bo ff q =
    OrderLimitOffsetLockSelect q $ \r ->
      ( r,
        MkOrderLimitOffsetLockCfg
          { offsetClause = off,
            fetchFirstClause = ff,
            orderByClause = bo r,
            lockingClause = NoLockingClause
          }
      )

instance OrderOffsetLimitQuery AggregateQuery where
  type OrderOffsetLimitScope AggregateQuery = Aggregated
  offsetOrderLimit_ off bo ff q =
    OrderLimitOffsetLockAggregated q $ \r ->
      ( r,
        MkOrderLimitOffsetLockCfg
          { offsetClause = off,
            fetchFirstClause = ff,
            orderByClause = bo r,
            lockingClause = NoLockingClause
          }
      )

orderLimit_ ::
  ( ValidOrderLimitOffsetLock ff obu Nothing,
    OrderOffsetLimitQuery query
  ) =>
  (t -> OrderByClause (OrderOffsetLimitScope query) obu) ->
  FetchFirstClause ff ->
  query t ->
  OrderLimitOffsetLock (OrderOffsetLimitScope query) t
orderLimit_ = offsetOrderLimit_ Nothing

limit_ ::
  ( ValidOrderLimitOffsetLock ff NoOrderBy Nothing,
    OrderOffsetLimitQuery query
  ) =>
  FetchFirstClause ff ->
  query t ->
  OrderLimitOffsetLock (OrderOffsetLimitScope query) t
limit_ = orderLimit_ (const NoOrder)

offsetLimit_ ::
  ( ValidOrderLimitOffsetLock ff NoOrderBy Nothing,
    OrderOffsetLimitQuery query
  ) =>
  Maybe Int64 ->
  FetchFirstClause ff ->
  query t ->
  OrderLimitOffsetLock (OrderOffsetLimitScope query) t
offsetLimit_ offset = offsetOrderLimit_ offset (const NoOrder)

renderOrderLimitOffsetLock ::
  (SelectList result) =>
  OrderLimitOffsetLock expectedScope (result (SqlExpr expectedScope)) ->
  QueryWriter ()
renderOrderLimitOffsetLock = \case
  OrderLimitOffsetLockSelect sl buildRes -> do
    (selectRes, finalState) <- runStateT (unsafeGetSelectM sl) mempty
    let (res, cfg) = buildRes selectRes
    renderSelectClause res
    renderFromWhere finalState
    renderCfg cfg
  OrderLimitOffsetLockAggregated aq buildRes ->
    case aq of
      AggregateGroupBy q gb h p -> do
        (rawResult, finalState) <- runStateT (unsafeGetSelectM q) mempty
        let groupedHKD = gb rawResult
            aggregatedGroupedHKD = ffmap coerce groupedHKD
            aggregateSetRow = ffmap coerce rawResult
            combined = aggregatedGroupedHKD :--: aggregateSetRow
            aggRes = p combined
            (res, cfg) = buildRes aggRes

        renderSelectClause res
        renderFromWhere finalState

        " GROUP BY "
        let gbSyns = ffoldMap (Written . unsafeGetSqlExpr) groupedHKD
        for_ (fromCommaSepWritten gbSyns) writeSyntax

        for_ h $ \havingF -> do
          " HAVING "
          writeSyntax (unsafeGetSqlExpr $ havingF combined)

        renderCfg cfg
      AggregateEntireQuery q p -> do
        (rawResult, finalState) <- runStateT (unsafeGetSelectM q) mempty
        let aggregateSetRow = ffmap coerce rawResult
            aggRes = p aggregateSetRow
            (res, cfg) = buildRes aggRes

        renderSelectClause res
        renderFromWhere finalState
        renderCfg cfg

renderSelectClause :: (SelectList sl) => sl (SqlExpr scope) -> QueryWriter ()
renderSelectClause res = do
  "SELECT"
  for_ (aliasedColumnList res) $ \syn -> do
    " "
    writeSyntax syn

renderFromWhere :: SelectMState -> QueryWriter ()
renderFromWhere finalState = do
  for_ (fromCommaSepWritten $ foldMap Written finalState.fromSyntaxes) $ \syns -> do
    " FROM "
    writeSyntax syns
  for_ (writeAnds finalState.whereSyntaxes) $ \act -> do
    " WHERE "
    act

renderCfg :: OrderLimitOffsetLockCfg scope ti obu slu -> QueryWriter ()
renderCfg cfg = do
  renderOrderBy cfg.orderByClause
  renderOffset cfg.offsetClause
  renderFetch cfg.fetchFirstClause
  renderLocking cfg.lockingClause

renderOrderBy :: OrderByClause scope obu -> QueryWriter ()
renderOrderBy = \case
  NoOrder -> return ()
  OrderQueryBy oc -> do
    " ORDER BY "
    writeSyntax $ unsafeOrderClauseToSyntax oc

renderOffset :: Maybe Int64 -> QueryWriter ()
renderOffset = \case
  Nothing -> return ()
  Just i -> do
    " OFFSET "
    writeSyntax $ fromString (show i)
    " ROWS"

renderFetch :: FetchFirstClause ti -> QueryWriter ()
renderFetch = \case
  NoFetchFirstClause -> return ()
  FetchFirstClauseOnly i -> do
    " FETCH FIRST "
    writeSyntax $ fromString (show i)
    " ROWS ONLY"
  FetchFirstClauseWithTies i -> do
    " FETCH FIRST "
    writeSyntax $ fromString (show i)
    " ROWS WITH TIES"

renderLocking :: LockingClause slu -> QueryWriter ()
renderLocking = \case
  NoLockingClause -> return ()
  LockingClause kind wait -> do
    " "
    writeSyntax $ lockKindToSyntax kind
    writeSyntax $ lockWaitToSyntax wait

lockKindToSyntax :: LockKind -> Syntax
lockKindToSyntax = \case
  ForUpdate -> "FOR UPDATE"
  ForNoKeyUpdate -> "FOR NO KEY UPDATE"
  ForShare -> "FOR SHARE"
  ForKeyShare -> "FOR KEY SHARE"

lockWaitToSyntax :: LockWait usage -> Syntax
lockWaitToSyntax = \case
  Wait -> ""
  NoWait -> " NOWAIT"
  SkipLocked -> " SKIP LOCKED"
