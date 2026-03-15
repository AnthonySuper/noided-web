{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.DateTimeSpec (spec) where

import Data.Int (Int32)
import Data.Text (unpack)
import Data.Time (Day, DiffTime, LocalTime, UTCTime)
import Noided.Sql.Internal.SqlExpr.DateTime
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec

renderExpr :: SqlExpr scope t -> String
renderExpr = unpack . renderSyntaxToTextNumberedBinds . unsafeGetSqlExpr

spec :: Spec
spec = do
  describe "DateTime Expressions" $ do
    let d = UnsafeMkSqlExpr "d" :: SqlExpr NormalQuery (NonNullT Day)
        ts = UnsafeMkSqlExpr "ts" :: SqlExpr NormalQuery (NonNullT LocalTime)
        tstz = UnsafeMkSqlExpr "tstz" :: SqlExpr NormalQuery (NonNullT UTCTime)
        iv = UnsafeMkSqlExpr "iv" :: SqlExpr NormalQuery (NonNullT DiffTime)
        ndays = UnsafeMkSqlExpr "n" :: SqlExpr NormalQuery (NonNullT Int32)
        ep = UnsafeMkSqlExpr "ep" :: SqlExpr NormalQuery (NonNullT Double)

    describe "current date and time" $ do
      it "renders now_" $ renderExpr now_ `shouldBe` "NOW()"
      it "renders currentDate_" $ renderExpr currentDate_ `shouldBe` "CURRENT_DATE"
      it "renders currentTimestamp_" $ renderExpr currentTimestamp_ `shouldBe` "CURRENT_TIMESTAMP"
      it "renders localTimestamp_" $ renderExpr localTimestamp_ `shouldBe` "LOCALTIMESTAMP"
      it "renders clockTimestamp_" $ renderExpr clockTimestamp_ `shouldBe` "CLOCK_TIMESTAMP()"
      it "renders statementTimestamp_" $ renderExpr statementTimestamp_ `shouldBe` "STATEMENT_TIMESTAMP()"
      it "renders transactionTimestamp_" $ renderExpr transactionTimestamp_ `shouldBe` "TRANSACTION_TIMESTAMP()"

    describe "date arithmetic" $ do
      it "renders dateAddDays_" $ renderExpr (dateAddDays_ d ndays) `shouldBe` "(d) + (n)"
      it "renders dateSubDays_" $ renderExpr (dateSubDays_ d ndays) `shouldBe` "(d) - (n)"
      it "renders dateDiffDays_" $ renderExpr (dateDiffDays_ d d) `shouldBe` "(d) - (d)"
      it "renders dateAddInterval_" $ renderExpr (dateAddInterval_ d iv) `shouldBe` "(d) + (iv)"

    describe "interval arithmetic" $ do
      it "renders intervalAdd_" $ renderExpr (intervalAdd_ iv iv) `shouldBe` "(iv) + (iv)"
      it "renders intervalSub_" $ renderExpr (intervalSub_ iv iv) `shouldBe` "(iv) - (iv)"
      it "renders intervalNegate_" $ renderExpr (intervalNegate_ iv) `shouldBe` "-(iv)"

    describe "timestamp arithmetic" $ do
      it "renders timestampAddInterval_" $ renderExpr (timestampAddInterval_ ts iv) `shouldBe` "(ts) + (iv)"
      it "renders timestampSubInterval_" $ renderExpr (timestampSubInterval_ ts iv) `shouldBe` "(ts) - (iv)"
      it "renders timestampDiff_" $ renderExpr (timestampDiff_ ts ts) `shouldBe` "(ts) - (ts)"

    describe "timestamptz arithmetic" $ do
      it "renders timestamptzAddInterval_" $ renderExpr (timestamptzAddInterval_ tstz iv) `shouldBe` "(tstz) + (iv)"
      it "renders timestamptzSubInterval_" $ renderExpr (timestamptzSubInterval_ tstz iv) `shouldBe` "(tstz) - (iv)"
      it "renders timestamptzDiff_" $ renderExpr (timestamptzDiff_ tstz tstz) `shouldBe` "(tstz) - (tstz)"

    describe "extraction and truncation" $ do
      it "renders datePart_ for timestamp" $ renderExpr (datePart_ "year" ts) `shouldBe` "DATE_PART('year', ts)"
      it "renders datePart_ for timestamptz" $ renderExpr (datePart_ "epoch" tstz) `shouldBe` "DATE_PART('epoch', tstz)"
      it "renders dateTrunc_" $ renderExpr (dateTrunc_ "month" ts) `shouldBe` "DATE_TRUNC('month', ts)"
      it "renders dateTruncTz_" $ renderExpr (dateTruncTz_ "day" tstz) `shouldBe` "DATE_TRUNC('day', tstz)"
      it "renders dateTruncInterval_" $ renderExpr (dateTruncInterval_ "hour" iv) `shouldBe` "DATE_TRUNC('hour', iv)"

    describe "age functions" $ do
      it "renders age_" $ renderExpr (age_ ts ts) `shouldBe` "AGE(ts, ts)"
      it "renders ageTz_" $ renderExpr (ageTz_ tstz tstz) `shouldBe` "AGE(tstz, tstz)"

    describe "interval adjustment" $ do
      it "renders justifyDays_" $ renderExpr (justifyDays_ iv) `shouldBe` "JUSTIFY_DAYS(iv)"
      it "renders justifyHours_" $ renderExpr (justifyHours_ iv) `shouldBe` "JUSTIFY_HOURS(iv)"
      it "renders justifyInterval_" $ renderExpr (justifyInterval_ iv) `shouldBe` "JUSTIFY_INTERVAL(iv)"

    describe "conversion" $ do
      it "renders toTimestamp_" $ renderExpr (toTimestamp_ ep) `shouldBe` "TO_TIMESTAMP(ep)"

    describe "finiteness checks" $ do
      it "renders isFiniteDate_" $ renderExpr (isFiniteDate_ d) `shouldBe` "ISFINITE(d)"
      it "renders isFiniteTimestamp_" $ renderExpr (isFiniteTimestamp_ ts) `shouldBe` "ISFINITE(ts)"
      it "renders isFiniteTimestamptz_" $ renderExpr (isFiniteTimestamptz_ tstz) `shouldBe` "ISFINITE(tstz)"
      it "renders isFiniteInterval_" $ renderExpr (isFiniteInterval_ iv) `shouldBe` "ISFINITE(iv)"
