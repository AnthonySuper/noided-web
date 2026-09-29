{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.RowSpec (spec) where

import Data.HKD (Element (..))
import Data.Text (Text, unpack)
import Noided.Sql.Internal.SqlExpr.Row
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Noided.Sql.Internal.Type.Tie
import Test.Hspec
import Test.Hspec.Golden

renderGolden :: String -> SqlExpr NormalQuery (SqlT n r) -> Spec
renderGolden description expr =
  golden description (return syntaxString)
  where
    syntaxString = unpack (renderSyntaxToTextNumberedBinds (unsafeGetSqlExpr expr))

spec :: Spec
spec = describe "Row Expressions" $ do
  let a = unsafeMkAtom "a" :: SqlExpr NormalQuery (NonNullT Int)
      b = unsafeMkAtom "b" :: SqlExpr NormalQuery (NonNullT Text)
  renderGolden "row-single" (row_ (Element a))
  renderGolden "row-tuple" (row_ (Element a :-: Element b))
