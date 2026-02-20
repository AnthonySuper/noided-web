{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.TextSpec (spec) where

import Data.Int (Int32)
import Data.Text (Text, unpack)
import Noided.Sql.Internal.SqlExpr.Text
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec
import Test.Hspec.Golden

renderGolden :: String -> SqlExpr NormalQuery (SqlT n r) -> Spec
renderGolden description expr =
  golden (description) (return syntaxString)
  where
    syntaxString = unpack (renderSyntaxToTextNumberedBinds (unsafeGetSqlExpr expr))

spec :: Spec
spec = do
  describe "Text Expressions" $ do
    let s = UnsafeMkSqlExpr "s" :: SqlExpr NormalQuery (NonNullT Text)
        s2 = UnsafeMkSqlExpr "s2" :: SqlExpr NormalQuery (NonNullT Text)
        i1 = UnsafeMkSqlExpr "1" :: SqlExpr NormalQuery (NonNullT Int32)
        i2 = UnsafeMkSqlExpr "2" :: SqlExpr NormalQuery (NonNullT Int32)

    renderGolden "concatenation" (s <>. s2)
    renderGolden "lower" (lower_ s)
    renderGolden "upper" (upper_ s)
    renderGolden "length" (length_ s)
    renderGolden "trim" (trim_ s)
    renderGolden "ltrim" (ltrim_ s)
    renderGolden "rtrim" (rtrim_ s)
    renderGolden "substr" (substr_ s i1 i2)
    renderGolden "replace" (replace_ s s2 s)
