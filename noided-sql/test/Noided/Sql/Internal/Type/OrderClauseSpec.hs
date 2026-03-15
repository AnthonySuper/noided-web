{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.OrderClauseSpec (spec) where

import Data.Text (Text)
import Noided.Sql.Internal.Type.OrderClause
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec

checkSyntax :: OrderClause NormalQuery -> Text -> Expectation
checkSyntax clause expected =
  renderSyntaxToTextNumberedBinds (unsafeOrderClauseToSyntax clause) `shouldBe` expected

spec :: Spec
spec = do
  describe "OrderClause" $ do
    let col1 = UnsafeMkSqlExpr "col1" :: SqlExpr NormalQuery (NonNullT Int)
        col2 = UnsafeMkSqlExpr "col2" :: SqlExpr NormalQuery (NonNullT Int)

    it "renders ASC" $ do
      checkSyntax (asc_ col1) "col1 ASC NULLS LAST"

    it "renders DESC" $ do
      checkSyntax (desc_ col1) "col1 DESC NULLS FIRST"

    it "renders ASC NULLS FIRST" $ do
      checkSyntax (ascNullFirst_ col1) "col1 ASC NULLS FIRST"

    it "renders ASC NULLS LAST" $ do
      checkSyntax (ascNullsLast_ col1) "col1 ASC NULLS LAST"

    it "renders DESC NULLS FIRST" $ do
      checkSyntax (descNullsFirst_ col1) "col1 DESC NULLS FIRST"

    it "renders DESC NULLS LAST" $ do
      checkSyntax (descNullsLast_ col1) "col1 DESC NULLS LAST"

    it "renders multiple columns" $ do
      checkSyntax (asc_ col1 <> desc_ col2) "col1 ASC NULLS LAST, col2 DESC NULLS FIRST"

    it "renders multiple columns with specific nulls" $ do
      checkSyntax (ascNullFirst_ col1 <> descNullsLast_ col2) "col1 ASC NULLS FIRST, col2 DESC NULLS LAST"

    it "renders explicit orderBy_ and orderingBy_" $ do
      checkSyntax (orderBy_ col1 orderAsc) "col1 ASC NULLS LAST"
      checkSyntax (orderingBy_ orderDesc col1) "col1 DESC NULLS FIRST"
