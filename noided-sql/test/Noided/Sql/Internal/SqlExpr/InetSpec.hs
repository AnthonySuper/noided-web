{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.InetSpec (spec) where

import Data.IP (IPRange)
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack)
import Noided.Sql.Internal.SqlExpr.Inet
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec
import Test.Hspec.Golden

renderGolden :: String -> SqlExpr NormalQuery (SqlT n r) -> Spec
renderGolden description expr =
  golden description (return syntaxString)
  where
    syntaxString = unpack (renderSyntaxToTextNumberedBinds (unsafeGetSqlExpr expr))

spec :: Spec
spec = do
  describe "Inet Expressions" $ do
    let a = UnsafeMkSqlExpr "a" :: SqlExpr NormalQuery (NonNullT IPRange)
        b = UnsafeMkSqlExpr "b" :: SqlExpr NormalQuery (NonNullT IPRange)
        n = UnsafeMkSqlExpr "n" :: SqlExpr NormalQuery (NonNullT Int32)
        off = UnsafeMkSqlExpr "off" :: SqlExpr NormalQuery (NonNullT Int64)

    describe "containment operators" $ do
      renderGolden "is-contained-by" (a <<. b)
      renderGolden "is-contained-by-or-equals" (a <<=. b)
      renderGolden "contains" (a >>. b)
      renderGolden "contains-or-equals" (a >>=. b)
      renderGolden "overlaps" (inetOverlaps_ a b)

    describe "inet functions" $ do
      renderGolden "abbrev" (abbrev_ a)
      renderGolden "broadcast" (broadcast_ a)
      renderGolden "family" (family_ a)
      renderGolden "host" (host_ a)
      renderGolden "hostmask" (hostmask_ a)
      renderGolden "inet-merge" (inetMerge_ a b)
      renderGolden "inet-same-family" (inetSameFamily_ a b)
      renderGolden "masklen" (masklen_ a)
      renderGolden "netmask" (netmask_ a)
      renderGolden "network" (network_ a)
      renderGolden "set-masklen" (setMasklen_ a n)
      renderGolden "inet-to-text" (inetToText_ a)

    describe "inet arithmetic" $ do
      renderGolden "inet-add-offset" (inetAddOffset_ a off)
      renderGolden "inet-sub-offset" (inetSubOffset_ a off)
      renderGolden "inet-sub-inet" (inetSubInet_ a b)
