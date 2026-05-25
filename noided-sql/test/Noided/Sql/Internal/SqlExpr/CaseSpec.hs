{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.CaseSpec (spec) where

import Data.Text (unpack)
import Noided.Sql.Internal.SqlExpr.Case
import Noided.Sql.Internal.Type.CaseBranch (then_)
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax (renderSyntaxToTextNumberedBinds)
import Test.Hspec

renderExpr :: SqlExpr scope t -> String
renderExpr = unpack . renderSyntaxToTextNumberedBinds . unsafeGetSqlExpr

spec :: Spec
spec = describe "Case Expressions" $ do
  let cond1 = UnsafeMkSqlExpr "cond1" :: SqlExpr NormalQuery (NonNullT Bool)
      cond2 = UnsafeMkSqlExpr "cond2" :: SqlExpr NormalQuery (NonNullT Bool)
      val1 = UnsafeMkSqlExpr "val1" :: SqlExpr NormalQuery (NonNullT Int)
      val2 = UnsafeMkSqlExpr "val2" :: SqlExpr NormalQuery (NonNullT Int)
      elseVal = UnsafeMkSqlExpr "elseVal" :: SqlExpr NormalQuery (NonNullT Int)
      compared = UnsafeMkSqlExpr "x" :: SqlExpr NormalQuery (NonNullT Int)
      match1 = UnsafeMkSqlExpr "1" :: SqlExpr NormalQuery (NonNullT Int)
      match2 = UnsafeMkSqlExpr "2" :: SqlExpr NormalQuery (NonNullT Int)

  it "renders case_ with a single branch and else clause" $ do
    let branches = [cond1 `then_` val1]
    renderExpr (case_ branches elseVal) `shouldBe` "CASE WHEN cond1 THEN val1 ELSE elseVal END"

  it "renders case_ with multiple branches and else clause" $ do
    let branches = [cond1 `then_` val1, cond2 `then_` val2]
    renderExpr (case_ branches elseVal) `shouldBe` "CASE WHEN cond1 THEN val1 WHEN cond2 THEN val2 ELSE elseVal END"

  it "renders caseNoElse_ with multiple branches" $ do
    let branches = [cond1 `then_` val1, cond2 `then_` val2]
    renderExpr (caseNoElse_ branches) `shouldBe` "CASE WHEN cond1 THEN val1 WHEN cond2 THEN val2 END"

  it "renders caseSimple_ with a compared value, branches and else clause" $ do
    let branches = [match1 `then_` val1, match2 `then_` val2]
    renderExpr (caseSimple_ compared branches elseVal) `shouldBe` "CASE x WHEN 1 THEN val1 WHEN 2 THEN val2 ELSE elseVal END"
