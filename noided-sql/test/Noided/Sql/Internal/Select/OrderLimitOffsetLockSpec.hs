{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Noided.Sql.Internal.Select.OrderLimitOffsetLockSpec (spec) where

import Data.HKD
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text (unpack)
import GHC.Generics
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Class.Query (writeQuerySyntax)
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Select.AggregateQuery (AggregateQuery, aggregate_)
import Noided.Sql.Internal.Select.OrderLimitOffsetLock
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.Type.AggregateExpr (agg, count_, sum_)
import Noided.Sql.Internal.Type.OrderClause
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec
import Test.Hspec.Golden

data Table1 f = Table1 {t1Id :: f (NonNullT Int64), t1Val :: f (NonNullT Int64)} deriving (Generic)

instance FFunctor Table1 where ffmap = ffmapDefault

instance FFoldable Table1 where ffoldMap = ffoldMapDefault

instance FTraversable Table1 where ftraverse = gftraverse

instance FZip Table1 where fzipWith = gfzipWith

instance NamedColumns Table1 where
  namedColumns = Table1 "id" "val"

data TableAgg f = TableAgg {aggId :: f (NonNullT Int64), aggVal :: f (NullableT Scientific)} deriving (Generic)

instance FFunctor TableAgg where ffmap = ffmapDefault

instance FFoldable TableAgg where ffoldMap = ffoldMapDefault

instance FTraversable TableAgg where ftraverse = gftraverse

instance FZip TableAgg where fzipWith = gfzipWith

instance NamedColumns TableAgg where
  namedColumns = TableAgg "id" "val"

renderGolden ::
  (SelectList sl) =>
  String ->
  OrderLimitOffsetLock scope (sl (SqlExpr scope)) ->
  Spec
renderGolden description ol =
  golden description (return syntaxString)
  where
    syntaxString = unpack (renderSyntaxToTextNumberedBinds (renderQueryWriter (writeQuerySyntax ol)))

spec :: Spec
spec = do
  describe "OrderLimitOffsetLock" $ do
    let col1 = UnsafeMkSqlExpr "col1" :: SqlExpr NormalQuery (NonNullT Int64)

    describe "SelectM" $ do
      let simpleSelect = select_ (Table1 col1 col1)

      renderGolden "Simple ORDER BY" $
        OrderLimitOffsetLockSelect simpleSelect $
          (,MkOrderLimitOffsetLockCfg
              Nothing
              NoFetchFirstClause
              (OrderQueryBy (asc_ col1))
              NoLockingClause)

      renderGolden "ORDER BY, OFFSET, FETCH FIRST" $
        OrderLimitOffsetLockSelect simpleSelect $
          (,MkOrderLimitOffsetLockCfg
              (Just 10)
              (FetchFirstClauseOnly 5)
              (OrderQueryBy (desc_ col1))
              NoLockingClause)

      renderGolden "ORDER BY, OFFSET, FETCH FIRST WITH TIES" $
        OrderLimitOffsetLockSelect simpleSelect $
          (,MkOrderLimitOffsetLockCfg
              (Just 10)
              (FetchFirstClauseWithTies 5)
              (OrderQueryBy (desc_ col1))
              NoLockingClause)

      renderGolden "Locking FOR UPDATE" $
        OrderLimitOffsetLockSelect simpleSelect $
          (,MkOrderLimitOffsetLockCfg
              Nothing
              NoFetchFirstClause
              NoOrder
              (LockingClause ForUpdate Wait))

      renderGolden "Locking FOR SHARE SKIP LOCKED" $
        OrderLimitOffsetLockSelect simpleSelect $
          (,MkOrderLimitOffsetLockCfg
              Nothing
              NoFetchFirstClause
              NoOrder
              (LockingClause ForShare SkipLocked))

    describe "AggregateQuery" $ do
      let simpleAgg = aggregate_ (\r -> TableAgg (agg $ count_ r.t1Id) (agg $ sum_ r.t1Val)) (select_ (Table1 col1 col1))

      renderGolden "Aggregate with ORDER BY" $
        OrderLimitOffsetLockAggregated simpleAgg $ \res ->
          (res, MkOrderLimitOffsetLockCfg Nothing NoFetchFirstClause (OrderQueryBy (desc_ res.aggVal)) NoLockingClause)
