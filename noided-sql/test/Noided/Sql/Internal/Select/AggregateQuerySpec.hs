{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Select.AggregateQuerySpec (spec) where

import Data.Coerce (coerce)
import Data.HKD
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (unpack)
import GHC.Generics
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Select.AggregateQuery
import Noided.Sql.Internal.Select.FromClause
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.SqlExpr.Bool ((==.))
import Noided.Sql.Internal.Type.AggregateExpr
import Noided.Sql.Internal.Type.PGArray
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Noided.Sql.Internal.Type.Tie
import Test.Hspec
import Test.Hspec.Golden

data Table1 f = Table1 {t1Id :: f (NonNullT Int64), t1Val :: f (NonNullT Int64)} deriving (Generic)

instance FFunctor Table1 where ffmap = ffmapDefault

instance FFoldable Table1 where ffoldMap = ffoldMapDefault

instance FTraversable Table1 where ftraverse = gftraverse

instance FZip Table1 where fzipWith = gfzipWith

instance NamedColumns Table1 where
  namedColumns = Table1 "id" "val"

data Stats f = Stats
  { statsCount :: f (NonNullT Int64),
    statsSum :: f (NullableT Scientific)
  }
  deriving (Generic)

instance FFunctor Stats where ffmap = ffmapDefault

instance FFoldable Stats where ffoldMap = ffoldMapDefault

instance FTraversable Stats where ftraverse = gftraverse

instance FZip Stats where fzipWith = gfzipWith

instance NamedColumns Stats where
  namedColumns = Stats "count" "sum"

data GroupedStats f = GroupedStats
  { gsId :: f (NonNullT Int64),
    gsCount :: f (NonNullT Int64)
  }
  deriving (Generic)

instance FFunctor GroupedStats where ffmap = ffmapDefault

instance FFoldable GroupedStats where ffoldMap = ffoldMapDefault

instance FTraversable GroupedStats where ftraverse = gftraverse

instance FZip GroupedStats where fzipWith = gfzipWith

instance NamedColumns GroupedStats where
  namedColumns = GroupedStats "id" "count"

data ArrayStats f = ArrayStats
  { asId :: f (NonNullT Int64),
    asVals :: f (NullableT (PGArray (NonNullT Int64)))
  }
  deriving (Generic)

instance FFunctor ArrayStats where ffmap = ffmapDefault

instance FFoldable ArrayStats where ffoldMap = ffoldMapDefault

instance FTraversable ArrayStats where ftraverse = gftraverse

instance FZip ArrayStats where fzipWith = gfzipWith

instance NamedColumns ArrayStats where
  namedColumns = ArrayStats "id" "vals"

data BoolStats f = BoolStats
  { bsAny :: f (NullableT Bool),
    bsEvery :: f (NullableT Bool)
  }
  deriving (Generic)

instance FFunctor BoolStats where ffmap = ffmapDefault

instance FFoldable BoolStats where ffoldMap = ffoldMapDefault

instance FTraversable BoolStats where ftraverse = gftraverse

instance FZip BoolStats where fzipWith = gfzipWith

instance NamedColumns BoolStats where
  namedColumns = BoolStats "any" "every"

renderAggregateGolden ::
  (FZip sl, FTraversable sl, NamedColumns sl) =>
  String ->
  AggregateQuery any (sl (SqlExpr Aggregated)) ->
  Spec
renderAggregateGolden description aq =
  golden description (return syntaxString)
  where
    syntaxString = unpack (renderSyntaxToTextNumberedBinds syntax)
    syntax = renderQueryWriter (renderAggregateQuery aq)

spec :: Spec
spec = do
  describe "AggregateEntireQuery" $ do
    renderAggregateGolden "Simple count and sum over a table" $
      aggregate_
        (\(Table1 _ val) -> Stats (agg_ $ count_ val) (agg_ $ sum_ val))
        (addFrom_ (fromBase_ $ select_ $ Table1 (unsafeMkAtom "id") (unsafeMkAtom "val")))

  describe "AggregateBoolean" $ do
    renderAggregateGolden "any_ renders BOOL_OR" $
      aggregate_
        (\(Table1 _ val) -> BoolStats (agg_ $ any_ (val ==. unsafeMkAtom "1")) (agg_ $ every_ (val ==. unsafeMkAtom "1")))
        (addFrom_ (fromBase_ $ select_ $ Table1 (unsafeMkAtom "id") (unsafeMkAtom "val")))

  describe "AggregateGroupBy" $ do
    let baseQuery = addFrom_ (fromBase_ $ select_ $ Table1 (unsafeMkAtom "id") (unsafeMkAtom "val"))

    renderAggregateGolden "Group by id and count vals" $
      groupBy_
        (\t1 -> Element t1.t1Id)
        (\(Element idAgg :--: t1Agg) -> GroupedStats (coerce idAgg) (agg_ $ count_ t1Agg.t1Val))
        baseQuery

    renderAggregateGolden "Group by id with HAVING clause" $
      groupByHaving_
        (\t1 -> Element t1.t1Id)
        (\(_ :--: t1Agg) -> agg_ (count_ t1Agg.t1Val) ==. unsafeMkAtom "5")
        (\(Element idAgg :--: t1Agg) -> GroupedStats (coerce idAgg) (agg_ $ count_ t1Agg.t1Val))
        baseQuery

    renderAggregateGolden "Group by id and array_agg vals" $
      groupBy_
        (\t1 -> Element t1.t1Id)
        (\(Element idAgg :--: t1Agg) -> ArrayStats (coerce idAgg) (agg_ $ arrayAgg_ t1Agg.t1Val))
        baseQuery

  describe "stddev_ / variance_ result types" $ do
    -- These only need to compile: Postgres returns numeric for integer/numeric input, double for float input.
    it "returns numeric for integer input" $ do
      let _stddevInt :: SqlExpr s (SqlT n Int32) -> AggregateExpr s (NullableT Scientific)
          _stddevInt = stddev_
          _varInt :: SqlExpr s (SqlT n Int64) -> AggregateExpr s (NullableT Scientific)
          _varInt = varPop_
      True `shouldBe` True
    it "returns double for double input" $ do
      let _stddevDouble :: SqlExpr s (SqlT n Double) -> AggregateExpr s (NullableT Double)
          _stddevDouble = stddevSamp_
      True `shouldBe` True
