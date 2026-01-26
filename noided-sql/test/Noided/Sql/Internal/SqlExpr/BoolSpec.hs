{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.BoolSpec (spec) where

import Data.Text (unpack)
import Noided.Sql.Internal.SqlExpr.Bool
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec

renderExpr :: SqlExpr scope t -> String
renderExpr = unpack . renderSyntaxToTextNumberedBinds . unsafeGetSqlExpr

spec :: Spec
spec = describe "Bool Expressions" $ do
  it "renders true_" $ do
    renderExpr true_ `shouldBe` "TRUE"

  it "renders false_" $ do
    renderExpr false_ `shouldBe` "FALSE"

  describe "binary operators" $ do
    let a = UnsafeMkSqlExpr "a" :: SqlExpr NormalQuery (NonNullT Bool)
    let b = UnsafeMkSqlExpr "b" :: SqlExpr NormalQuery (NonNullT Bool)

    it "renders &&. correctly" $ do
      renderExpr (a &&. b) `shouldBe` "(a) AND (b)"

    it "renders ||. correctly" $ do
      renderExpr (a ||. b) `shouldBe` "(a) OR (b)"

    it "associates &&. to the right" $ do
      let c = UnsafeMkSqlExpr "c" :: SqlExpr NormalQuery (NonNullT Bool)
      renderExpr (a &&. b &&. c) `shouldBe` "(a) AND ((b) AND (c))"
    
    it "associates ||. to the right" $ do
      let c = UnsafeMkSqlExpr "c" :: SqlExpr NormalQuery (NonNullT Bool)
      renderExpr (a ||. b ||. c) `shouldBe` "(a) OR ((b) OR (c))"
