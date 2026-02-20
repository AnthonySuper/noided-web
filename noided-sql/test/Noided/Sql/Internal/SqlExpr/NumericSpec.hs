{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.NumericSpec (spec) where

import Data.Int (Int32)
import Data.Text (unpack)
import Noided.Sql.Internal.SqlExpr.Numeric
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec
import Test.Hspec.Golden

renderGolden :: String -> SqlExpr NormalQuery (NonNullT Int32) -> Spec
renderGolden description expr =
  golden description (return syntaxString)
  where
    syntaxString = unpack (renderSyntaxToTextNumberedBinds (unsafeGetSqlExpr expr))

spec :: Spec
spec = do
  describe "Numeric Expressions" $ do
    let a = UnsafeMkSqlExpr "a" :: SqlExpr NormalQuery (NonNullT Int32)
        b = UnsafeMkSqlExpr "b" :: SqlExpr NormalQuery (NonNullT Int32)

    renderGolden "addition" (a +. b)
    renderGolden "subtraction" (a -. b)
    renderGolden "multiplication" (a *. b)
    renderGolden "division" (a /. b)
    renderGolden "abs" (abs_ a)
    renderGolden "negate" (negate_ a)
    renderGolden "complex expression" ((a +. b) *. (a -. b))
