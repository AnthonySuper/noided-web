{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.FullTextSpec (spec) where

import Data.Text (Text)
import Noided.Sql.Internal.SqlExpr.FullText
import Noided.Sql.Internal.Type.PGFullTextSearchWeight
import Noided.Sql.Internal.Type.PGRegConfig
import Noided.Sql.Internal.Type.PGTSQuery
import Noided.Sql.Internal.Type.PGTSVector
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec

renderTS :: SqlExpr NormalQuery (SqlT n r) -> Text
renderTS = renderSyntaxToTextNumberedBinds . unsafeGetSqlExpr

spec :: Spec
spec = do
  describe "Full-Text Search Expressions" $ do
    let v = UnsafeMkSqlExpr "v" :: SqlExpr NormalQuery (NonNullT PGTSVector)
        v2 = UnsafeMkSqlExpr "v2" :: SqlExpr NormalQuery (NonNullT PGTSVector)
        q = UnsafeMkSqlExpr "q" :: SqlExpr NormalQuery (NonNullT PGTSQuery)
        q2 = UnsafeMkSqlExpr "q2" :: SqlExpr NormalQuery (NonNullT PGTSQuery)
        cfg = UnsafeMkSqlExpr "cfg" :: SqlExpr NormalQuery (NonNullT PGRegConfig)
        txt = UnsafeMkSqlExpr "txt" :: SqlExpr NormalQuery (NonNullT Text)

    it "renders match operator" $ do
      renderTS (v @@. q) `shouldBe` "(v) @@ (q)"

    it "renders tsvector concatenation" $ do
      renderTS (concatTSVector_ v v2) `shouldBe` "(v) || (v2)"

    it "renders tsquery AND" $ do
      renderTS (tsAnd_ q q2) `shouldBe` "(q) & (q2)"

    it "renders tsquery OR" $ do
      renderTS (tsOr_ q q2) `shouldBe` "(q) | (q2)"

    it "renders tsquery NOT" $ do
      renderTS (tsNot_ q) `shouldBe` "! (q)"

    it "renders to_tsvector" $ do
      renderTS (toTSVector_ txt) `shouldBe` "to_tsvector(txt)"

    it "renders to_tsvector with config" $ do
      renderTS (toTSVectorWithConfig_ cfg txt) `shouldBe` "to_tsvector(cfg, txt)"

    it "renders to_tsquery" $ do
      renderTS (toTSQuery_ txt) `shouldBe` "to_tsquery(txt)"

    it "renders to_tsquery with config" $ do
      renderTS (toTSQueryWithConfig_ cfg txt) `shouldBe` "to_tsquery(cfg, txt)"

    it "renders plainto_tsquery" $ do
      renderTS (plainToTSQuery_ txt) `shouldBe` "plainto_tsquery(txt)"

    it "renders plainto_tsquery with config" $ do
      renderTS (plainToTSQueryWithConfig_ cfg txt) `shouldBe` "plainto_tsquery(cfg, txt)"

    it "renders phraseto_tsquery" $ do
      renderTS (phraseToTSQuery_ txt) `shouldBe` "phraseto_tsquery(txt)"

    it "renders phraseto_tsquery with config" $ do
      renderTS (phraseToTSQueryWithConfig_ cfg txt) `shouldBe` "phraseto_tsquery(cfg, txt)"

    it "renders websearch_to_tsquery" $ do
      renderTS (websearchToTSQuery_ txt) `shouldBe` "websearch_to_tsquery(txt)"

    it "renders websearch_to_tsquery with config" $ do
      renderTS (websearchToTSQueryWithConfig_ cfg txt) `shouldBe` "websearch_to_tsquery(cfg, txt)"

    it "renders setweight" $ do
      renderTS (setWeight_ v WeightA) `shouldBe` "setweight(v, 'A')"
      renderTS (setWeight_ v WeightB) `shouldBe` "setweight(v, 'B')"
      renderTS (setWeight_ v WeightC) `shouldBe` "setweight(v, 'C')"
      renderTS (setWeight_ v WeightD) `shouldBe` "setweight(v, 'D')"

    it "renders ts_rank" $ do
      renderTS (tsRank_ v q) `shouldBe` "ts_rank(v, q)"

    it "renders ts_rank_cd" $ do
      renderTS (tsRankCd_ v q) `shouldBe` "ts_rank_cd(v, q)"
