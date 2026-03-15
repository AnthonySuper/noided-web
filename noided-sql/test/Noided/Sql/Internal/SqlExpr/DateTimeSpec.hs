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
import Test.Hspec.Golden

renderGolden :: String -> SqlExpr NormalQuery (SqlT n r) -> Spec
renderGolden description expr =
  golden description (return syntaxString)
  where
    syntaxString = unpack (renderSyntaxToTextNumberedBinds (unsafeGetSqlExpr expr))

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
      renderGolden "now" now_
      renderGolden "current-date" currentDate_
      renderGolden "current-timestamp" currentTimestamp_
      renderGolden "local-timestamp" localTimestamp_
      renderGolden "clock-timestamp" clockTimestamp_
      renderGolden "statement-timestamp" statementTimestamp_
      renderGolden "transaction-timestamp" transactionTimestamp_

    describe "date arithmetic" $ do
      renderGolden "date-add-days" (dateAddDays_ d ndays)
      renderGolden "date-sub-days" (dateSubDays_ d ndays)
      renderGolden "date-diff-days" (dateDiffDays_ d d)
      renderGolden "date-add-interval" (dateAddInterval_ d iv)

    describe "interval arithmetic" $ do
      renderGolden "interval-add" (intervalAdd_ iv iv)
      renderGolden "interval-sub" (intervalSub_ iv iv)
      renderGolden "interval-negate" (intervalNegate_ iv)

    describe "timestamp arithmetic" $ do
      renderGolden "timestamp-add-interval" (timestampAddInterval_ ts iv)
      renderGolden "timestamp-sub-interval" (timestampSubInterval_ ts iv)
      renderGolden "timestamp-diff" (timestampDiff_ ts ts)

    describe "timestamptz arithmetic" $ do
      renderGolden "timestamptz-add-interval" (timestamptzAddInterval_ tstz iv)
      renderGolden "timestamptz-sub-interval" (timestamptzSubInterval_ tstz iv)
      renderGolden "timestamptz-diff" (timestamptzDiff_ tstz tstz)

    describe "extraction and truncation" $ do
      renderGolden "date-part-year-timestamp" (datePart_ "year" ts)
      renderGolden "date-part-epoch-timestamptz" (datePart_ "epoch" tstz)
      renderGolden "date-trunc-month" (dateTrunc_ "month" ts)
      renderGolden "date-trunc-tz-day" (dateTruncTz_ "day" tstz)
      renderGolden "date-trunc-interval-hour" (dateTruncInterval_ "hour" iv)

    describe "age functions" $ do
      renderGolden "age-timestamp" (age_ ts ts)
      renderGolden "age-timestamptz" (ageTz_ tstz tstz)

    describe "interval adjustment" $ do
      renderGolden "justify-days" (justifyDays_ iv)
      renderGolden "justify-hours" (justifyHours_ iv)
      renderGolden "justify-interval" (justifyInterval_ iv)

    describe "conversion" $ do
      renderGolden "to-timestamp" (toTimestamp_ ep)

    describe "finiteness checks" $ do
      renderGolden "isfinite-date" (isFiniteDate_ d)
      renderGolden "isfinite-timestamp" (isFiniteTimestamp_ ts)
      renderGolden "isfinite-timestamptz" (isFiniteTimestamptz_ tstz)
      renderGolden "isfinite-interval" (isFiniteInterval_ iv)
