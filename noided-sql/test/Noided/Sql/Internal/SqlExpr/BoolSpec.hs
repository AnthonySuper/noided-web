{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.BoolSpec (spec) where

import Data.HKD (Element (..))
import Data.Text (unpack)
import Noided.Sql.Internal.Select.SelectM (SelectM)
import Noided.Sql.Internal.SqlExpr.Bool
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax (renderSyntaxToTextNumberedBinds)
import Test.Hspec
import Test.Hspec.Golden

renderExpr :: SqlExpr scope t -> String
renderExpr = unpack . renderSyntaxToTextNumberedBinds . unsafeGetSqlExpr

spec :: Spec
spec = describe "Bool Expressions" $ do
  it "renders true_" $ do
    renderExpr true_ `shouldBe` "TRUE"

  it "renders false_" $ do
    renderExpr false_ `shouldBe` "FALSE"

  it "renders not_" $ do
    let a = unsafeMkAtom "a" :: SqlExpr NormalQuery (NonNullT Bool)
    renderExpr (not_ a) `shouldBe` "NOT a"

  describe "binary operators" $ do
    let a = unsafeMkAtom "a" :: SqlExpr NormalQuery (NonNullT Bool)
    let b = unsafeMkAtom "b" :: SqlExpr NormalQuery (NonNullT Bool)

    it "renders &&. correctly" $ do
      renderExpr (a &&. b) `shouldBe` "a AND b"

    it "renders ||. correctly" $ do
      renderExpr (a ||. b) `shouldBe` "a OR b"

    it "associates &&. to the right" $ do
      let c = unsafeMkAtom "c" :: SqlExpr NormalQuery (NonNullT Bool)
      renderExpr (a &&. b &&. c) `shouldBe` "a AND (b AND c)"
    
    it "associates ||. to the right" $ do
      let c = unsafeMkAtom "c" :: SqlExpr NormalQuery (NonNullT Bool)
      renderExpr (a ||. b ||. c) `shouldBe` "a OR (b OR c)"

  describe "comparison operators" $ do
    let x = unsafeMkAtom "x" :: SqlExpr NormalQuery (NonNullT Int)
    let y = unsafeMkAtom "y" :: SqlExpr NormalQuery (NonNullT Int)

    it "renders ==." $ renderExpr (x ==. y) `shouldBe` "x = y"
    it "renders <." $ renderExpr (x <. y) `shouldBe` "x < y"
    it "renders >." $ renderExpr (x >. y) `shouldBe` "x > y"
    it "renders <=." $ renderExpr (x <=. y) `shouldBe` "x <= y"
    it "renders >=." $ renderExpr (x >=. y) `shouldBe` "x >= y"
    it "renders /=." $ renderExpr (x /=. y) `shouldBe` "x <> y"

  describe "null checks" $ do
    let n = unsafeMkAtom "n" :: SqlExpr NormalQuery (NullableT Int)
    it "renders isNull_" $ renderExpr (isNull_ n) `shouldBe` "n IS NULL"
    it "renders isNotNull_" $ renderExpr (isNotNull_ n) `shouldBe` "n IS NOT NULL"

  describe "exists_" $ do
    it "renders exists_ with a simple query" $ do
      let query = return (Element (unsafeMkAtom "1")) :: SelectM (Element (NonNullT Int) (SqlExpr NormalQuery))
      renderExpr (exists_ query) `shouldBe` "EXISTS (SELECT 1 AS e)"

    describe "exists_ golden" $ do
      let query = return (Element (unsafeMkAtom "1")) :: SelectM (Element (NonNullT Int) (SqlExpr NormalQuery))
      golden "exists-simple-query" (return (renderExpr (exists_ query)))
