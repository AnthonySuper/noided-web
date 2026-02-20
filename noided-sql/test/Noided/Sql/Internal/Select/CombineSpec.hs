{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Noided.Sql.Internal.Select.CombineSpec (spec) where

import Data.HKD
import Data.Int (Int64)
import Data.Text (unpack)
import GHC.Generics
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Select.Combine
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec
import Test.Hspec.Golden

-- Define a simple table structure for testing
data TestTable f = TestTable {col1 :: f (NonNullT Int64), col2 :: f (NonNullT Int64)}
  deriving (Generic)

instance FFunctor TestTable where ffmap = ffmapDefault

instance FFoldable TestTable where ffoldMap = ffoldMapDefault

instance FTraversable TestTable where ftraverse = gftraverse

instance FZip TestTable where fzipWith = gfzipWith

instance NamedColumns TestTable where namedColumns = TestTable "col1" "col2"

-- Helper query
q1 :: SelectM (TestTable (SqlExpr NormalQuery))
q1 = return $ TestTable (bindParam @Int64 1) (bindParam @Int64 2)

q2 :: SelectM (TestTable (SqlExpr NormalQuery))
q2 = return $ TestTable (bindParam @Int64 3) (bindParam @Int64 4)

q3 :: SelectM (TestTable (SqlExpr NormalQuery))
q3 = return $ TestTable (bindParam @Int64 5) (bindParam @Int64 6)

renderGolden ::
  (Query q) =>
  String ->
  q ->
  Spec
renderGolden description query =
  golden description (return syntaxString)
  where
    syntaxString = unpack (renderSyntaxToTextNumberedBinds syntax)
    syntax = renderQueryWriter (writeQuerySyntax query)

spec :: Spec
spec = describe "CombinedQueries" $ do
  renderGolden "union" $
    QueryCombineUnion (QueryCombineOf (CombineBase q1))
      <> QueryCombineUnion (QueryCombineOf (CombineBase q2))

  renderGolden "union-all" $
    QueryCombineUnionAll (QueryCombineOf (CombineBase q1))
      <> QueryCombineUnionAll (QueryCombineOf (CombineBase q2))

  renderGolden "intersect" $
    QueryCombineIntersect (QueryCombineOf (CombineBase q1))
      <> QueryCombineIntersect (QueryCombineOf (CombineBase q2))

  renderGolden "except" $
    QueryCombineExcept (QueryCombineOf (CombineBase q1))
      <> QueryCombineExcept (QueryCombineOf (CombineBase q2))

  renderGolden "chaining" $
    QueryCombineUnion (QueryCombineOf (CombineBase q1))
      <> QueryCombineUnion (QueryCombineOf (CombineBase q2))
      <> QueryCombineUnion (QueryCombineOf (CombineBase q3))
