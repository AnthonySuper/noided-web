{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Noided.Sql.Internal.Update.SetsSpec (spec) where

import Data.Text (unpack, Text)
import Noided.Row
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.MutationExpr
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.Syntax
import Noided.Sql.Internal.Update.Sets
import Test.Hspec

type TestTable =
  '[ "col1" :=> RegularColumn Text,
     "col2" :=> RegularColumn Text
   ]

testTableCols :: WrappedRow TestTable ColumnName
testTableCols = MkColumnName "col1" :::% MkColumnName "col2" :::% EmptyWrappedRow

renderSets :: ColumnUpdates TestTable -> String
renderSets updates =
  unpack $ renderSyntaxToTextNumberedBinds $ renderQueryWriter $ writeUpdateSets updates testTableCols

spec :: Spec
spec = describe "writeUpdateSets" $ do
  it "renders simple updates" $ do
    let updates = #col1 |= MutateVal (bindParam @Text "val1")
    renderSets updates `shouldBe` "\"col1\" = $1"

  it "renders multiple updates" $ do
    let updates = #col1 |= MutateVal (bindParam @Text "val1")
               <> #col2 |= MutateVal (bindParam @Text "val2")
    renderSets updates `shouldBe` "\"col1\" = $1, \"col2\" = $2"

  it "last one wins (deduplication)" $ do
    let updates = #col1 |= MutateVal (bindParam @Text "first")
               <> #col1 |= MutateVal (bindParam @Text "second")
    renderSets updates `shouldBe` "\"col1\" = $1"
