{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Select.AggregateQuerySpec (spec) where

import Data.Coerce (coerce)
import Data.HKD
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text (unpack)
import GHC.Generics
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Select.AggregateQuery
import Noided.Sql.Internal.Select.FromClause
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.SqlExpr.Bool ((==.))
import Noided.Sql.Internal.Type.AggregateExpr
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

renderAggregateGolden ::
  (FZip sl, FTraversable sl, NamedColumns sl) =>
  String ->
  AggregateQuery (sl (SqlExpr Aggregated)) ->
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
      AggregateEntireQuery
        (addFrom_ (fromBase_ $ select_ $ Table1 (UnsafeMkSqlExpr "id") (UnsafeMkSqlExpr "val")))
        (\(Table1 _ val) -> Stats (agg $ count_ val) (agg $ sum_ val))

  describe "AggregateGroupBy" $ do
    let baseQuery = addFrom_ (fromBase_ $ select_ $ Table1 (UnsafeMkSqlExpr "id") (UnsafeMkSqlExpr "val"))

    renderAggregateGolden "Group by id and count vals" $
      AggregateGroupBy
        baseQuery
        (\t1 -> Element t1.t1Id)
        Nothing
        (\(Element idAgg :--: t1Agg) -> GroupedStats (coerce idAgg) (agg $ count_ t1Agg.t1Val))

    renderAggregateGolden "Group by id with HAVING clause" $
      AggregateGroupBy
        baseQuery
        (\t1 -> Element t1.t1Id)
        (Just $ \(_ :--: t1Agg) -> agg (count_ t1Agg.t1Val) ==. UnsafeMkSqlExpr "5")
        (\(Element idAgg :--: t1Agg) -> GroupedStats (coerce idAgg) (agg $ count_ t1Agg.t1Val))
