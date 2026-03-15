{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.DateTime where

import Data.Int (Int32)
import Data.Time (Day, DiffTime, LocalTime, UTCTime)
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax (Syntax, syntaxFromText)

-- * Field enums

-- | Fields that can be extracted by @DATE_PART@.
data DatePartField
  = DPCentury
  | DPDay
  | DPDecade
  | DPDow
  | DPDoy
  | DPEpoch
  | DPHour
  | DPIsoDow
  | DPIsoYear
  | DPJulian
  | DPMicroseconds
  | DPMillennium
  | DPMilliseconds
  | DPMinute
  | DPMonth
  | DPQuarter
  | DPSecond
  | DPTimezone
  | DPTimezoneHour
  | DPTimezoneMinute
  | DPWeek
  | DPYear
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Fields that can be used with @DATE_TRUNC@.
data DateTruncField
  = DTMicroseconds
  | DTMilliseconds
  | DTSecond
  | DTMinute
  | DTHour
  | DTDay
  | DTWeek
  | DTMonth
  | DTQuarter
  | DTYear
  | DTDecade
  | DTCentury
  | DTMillennium
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

datePartFieldSyntax :: DatePartField -> Syntax
datePartFieldSyntax f = syntaxFromText $ case f of
  DPCentury -> "century"
  DPDay -> "day"
  DPDecade -> "decade"
  DPDow -> "dow"
  DPDoy -> "doy"
  DPEpoch -> "epoch"
  DPHour -> "hour"
  DPIsoDow -> "isodow"
  DPIsoYear -> "isoyear"
  DPJulian -> "julian"
  DPMicroseconds -> "microseconds"
  DPMillennium -> "millennium"
  DPMilliseconds -> "milliseconds"
  DPMinute -> "minute"
  DPMonth -> "month"
  DPQuarter -> "quarter"
  DPSecond -> "second"
  DPTimezone -> "timezone"
  DPTimezoneHour -> "timezone_hour"
  DPTimezoneMinute -> "timezone_minute"
  DPWeek -> "week"
  DPYear -> "year"

dateTruncFieldSyntax :: DateTruncField -> Syntax
dateTruncFieldSyntax f = syntaxFromText $ case f of
  DTMicroseconds -> "microseconds"
  DTMilliseconds -> "milliseconds"
  DTSecond -> "second"
  DTMinute -> "minute"
  DTHour -> "hour"
  DTDay -> "day"
  DTWeek -> "week"
  DTMonth -> "month"
  DTQuarter -> "quarter"
  DTYear -> "year"
  DTDecade -> "decade"
  DTCentury -> "century"
  DTMillennium -> "millennium"

-- * Current date / time functions (no arguments)

-- | SQL @NOW()@: current date and time (with time zone).
now_ :: SqlExpr scope (NonNullT UTCTime)
now_ = UnsafeMkSqlExpr "NOW()"

-- | SQL @CURRENT_DATE@: current date.
currentDate_ :: SqlExpr scope (NonNullT Day)
currentDate_ = UnsafeMkSqlExpr "CURRENT_DATE"

-- | SQL @CURRENT_TIMESTAMP@: current date and time (with time zone).
currentTimestamp_ :: SqlExpr scope (NonNullT UTCTime)
currentTimestamp_ = UnsafeMkSqlExpr "CURRENT_TIMESTAMP"

-- | SQL @LOCALTIMESTAMP@: current date and time (without time zone).
localTimestamp_ :: SqlExpr scope (NonNullT LocalTime)
localTimestamp_ = UnsafeMkSqlExpr "LOCALTIMESTAMP"

-- | SQL @CLOCK_TIMESTAMP()@: current date and time (changes during statement execution).
clockTimestamp_ :: SqlExpr scope (NonNullT UTCTime)
clockTimestamp_ = UnsafeMkSqlExpr "CLOCK_TIMESTAMP()"

-- | SQL @STATEMENT_TIMESTAMP()@: current date and time at the start of the current statement.
statementTimestamp_ :: SqlExpr scope (NonNullT UTCTime)
statementTimestamp_ = UnsafeMkSqlExpr "STATEMENT_TIMESTAMP()"

-- | SQL @TRANSACTION_TIMESTAMP()@: current date and time at the start of the current transaction.
transactionTimestamp_ :: SqlExpr scope (NonNullT UTCTime)
transactionTimestamp_ = UnsafeMkSqlExpr "TRANSACTION_TIMESTAMP()"

-- * Date arithmetic

-- | SQL @date + integer@: add a number of days to a date.
dateAddDays_ ::
  SqlExpr scope (SqlT lhsN Day) ->
  SqlExpr scope (SqlT rhsN Int32) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Day)
dateAddDays_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") + (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @date - integer@: subtract a number of days from a date.
dateSubDays_ ::
  SqlExpr scope (SqlT lhsN Day) ->
  SqlExpr scope (SqlT rhsN Int32) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Day)
dateSubDays_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") - (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @date - date@: compute the number of days between two dates.
dateDiffDays_ ::
  SqlExpr scope (SqlT lhsN Day) ->
  SqlExpr scope (SqlT rhsN Day) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Int32)
dateDiffDays_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") - (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @date + interval@: add an interval to a date, producing a timestamp.
dateAddInterval_ ::
  SqlExpr scope (SqlT lhsN Day) ->
  SqlExpr scope (SqlT rhsN DiffTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) LocalTime)
dateAddInterval_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") + (" <> unsafeGetSqlExpr b <> ")")

-- * Interval arithmetic

-- | SQL @interval + interval@: add two intervals.
intervalAdd_ ::
  SqlExpr scope (SqlT lhsN DiffTime) ->
  SqlExpr scope (SqlT rhsN DiffTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) DiffTime)
intervalAdd_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") + (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @interval - interval@: subtract one interval from another.
intervalSub_ ::
  SqlExpr scope (SqlT lhsN DiffTime) ->
  SqlExpr scope (SqlT rhsN DiffTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) DiffTime)
intervalSub_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") - (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @- interval@: negate an interval.
intervalNegate_ ::
  SqlExpr scope (SqlT n DiffTime) ->
  SqlExpr scope (SqlT n DiffTime)
intervalNegate_ a = UnsafeMkSqlExpr ("-(" <> unsafeGetSqlExpr a <> ")")

-- * Timestamp arithmetic

-- | SQL @timestamp + interval@: add an interval to a timestamp.
timestampAddInterval_ ::
  SqlExpr scope (SqlT lhsN LocalTime) ->
  SqlExpr scope (SqlT rhsN DiffTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) LocalTime)
timestampAddInterval_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") + (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @timestamp - interval@: subtract an interval from a timestamp.
timestampSubInterval_ ::
  SqlExpr scope (SqlT lhsN LocalTime) ->
  SqlExpr scope (SqlT rhsN DiffTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) LocalTime)
timestampSubInterval_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") - (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @timestamp - timestamp@: compute the interval between two timestamps.
timestampDiff_ ::
  SqlExpr scope (SqlT lhsN LocalTime) ->
  SqlExpr scope (SqlT rhsN LocalTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) DiffTime)
timestampDiff_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") - (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @timestamptz + interval@: add an interval to a timestamptz.
timestamptzAddInterval_ ::
  SqlExpr scope (SqlT lhsN UTCTime) ->
  SqlExpr scope (SqlT rhsN DiffTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) UTCTime)
timestamptzAddInterval_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") + (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @timestamptz - interval@: subtract an interval from a timestamptz.
timestamptzSubInterval_ ::
  SqlExpr scope (SqlT lhsN UTCTime) ->
  SqlExpr scope (SqlT rhsN DiffTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) UTCTime)
timestamptzSubInterval_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") - (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @timestamptz - timestamptz@: compute the interval between two timestamptz values.
timestamptzDiff_ ::
  SqlExpr scope (SqlT lhsN UTCTime) ->
  SqlExpr scope (SqlT rhsN UTCTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) DiffTime)
timestamptzDiff_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") - (" <> unsafeGetSqlExpr b <> ")")

-- * Extraction and truncation functions

-- | SQL @DATE_PART(field, source)@: extract a date/time field as a floating-point number.
datePart_ ::
  DatePartField ->
  SqlExpr scope (SqlT n a) ->
  SqlExpr scope (SqlT n Double)
datePart_ field ts =
  UnsafeMkSqlExpr ("DATE_PART('" <> datePartFieldSyntax field <> "', " <> unsafeGetSqlExpr ts <> ")")

-- | SQL @DATE_TRUNC(field, source)@: truncate a timestamp to the given precision.
dateTrunc_ ::
  DateTruncField ->
  SqlExpr scope (SqlT n LocalTime) ->
  SqlExpr scope (SqlT n LocalTime)
dateTrunc_ field ts =
  UnsafeMkSqlExpr ("DATE_TRUNC('" <> dateTruncFieldSyntax field <> "', " <> unsafeGetSqlExpr ts <> ")")

-- | SQL @DATE_TRUNC(field, source)@: truncate a timestamptz to the given precision.
dateTruncTz_ ::
  DateTruncField ->
  SqlExpr scope (SqlT n UTCTime) ->
  SqlExpr scope (SqlT n UTCTime)
dateTruncTz_ field ts =
  UnsafeMkSqlExpr ("DATE_TRUNC('" <> dateTruncFieldSyntax field <> "', " <> unsafeGetSqlExpr ts <> ")")

-- | SQL @DATE_TRUNC(field, source)@: truncate an interval to the given precision.
dateTruncInterval_ ::
  DateTruncField ->
  SqlExpr scope (SqlT n DiffTime) ->
  SqlExpr scope (SqlT n DiffTime)
dateTruncInterval_ field iv =
  UnsafeMkSqlExpr ("DATE_TRUNC('" <> dateTruncFieldSyntax field <> "', " <> unsafeGetSqlExpr iv <> ")")

-- * Age functions

-- | SQL @AGE(timestamp, timestamp)@: compute the interval between two timestamps,
-- using years and months rather than just days.
age_ ::
  SqlExpr scope (SqlT lhsN LocalTime) ->
  SqlExpr scope (SqlT rhsN LocalTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) DiffTime)
age_ a b = UnsafeMkSqlExpr ("AGE(" <> unsafeGetSqlExpr a <> ", " <> unsafeGetSqlExpr b <> ")")

-- | SQL @AGE(timestamptz, timestamptz)@: compute the interval between two timestamptz values,
-- using years and months rather than just days.
ageTz_ ::
  SqlExpr scope (SqlT lhsN UTCTime) ->
  SqlExpr scope (SqlT rhsN UTCTime) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) DiffTime)
ageTz_ a b = UnsafeMkSqlExpr ("AGE(" <> unsafeGetSqlExpr a <> ", " <> unsafeGetSqlExpr b <> ")")

-- * Interval adjustment functions

-- | SQL @JUSTIFY_DAYS(interval)@: convert multiples of 30 days to months.
justifyDays_ ::
  SqlExpr scope (SqlT n DiffTime) ->
  SqlExpr scope (SqlT n DiffTime)
justifyDays_ a = UnsafeMkSqlExpr ("JUSTIFY_DAYS(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @JUSTIFY_HOURS(interval)@: convert multiples of 24 hours to days.
justifyHours_ ::
  SqlExpr scope (SqlT n DiffTime) ->
  SqlExpr scope (SqlT n DiffTime)
justifyHours_ a = UnsafeMkSqlExpr ("JUSTIFY_HOURS(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @JUSTIFY_INTERVAL(interval)@: adjust interval using @JUSTIFY_DAYS@ and @JUSTIFY_HOURS@.
justifyInterval_ ::
  SqlExpr scope (SqlT n DiffTime) ->
  SqlExpr scope (SqlT n DiffTime)
justifyInterval_ a = UnsafeMkSqlExpr ("JUSTIFY_INTERVAL(" <> unsafeGetSqlExpr a <> ")")

-- * Conversion functions

-- | SQL @TO_TIMESTAMP(double precision)@: convert a Unix epoch (seconds since 1970-01-01) to a timestamptz.
toTimestamp_ ::
  SqlExpr scope (SqlT n Double) ->
  SqlExpr scope (SqlT n UTCTime)
toTimestamp_ a = UnsafeMkSqlExpr ("TO_TIMESTAMP(" <> unsafeGetSqlExpr a <> ")")

-- * Finiteness checks

-- | SQL @ISFINITE(date)@: test for a finite date (not +/-infinity).
isFiniteDate_ ::
  SqlExpr scope (SqlT n Day) ->
  SqlExpr scope (SqlT n Bool)
isFiniteDate_ a = UnsafeMkSqlExpr ("ISFINITE(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @ISFINITE(timestamp)@: test for a finite timestamp (not +/-infinity).
isFiniteTimestamp_ ::
  SqlExpr scope (SqlT n LocalTime) ->
  SqlExpr scope (SqlT n Bool)
isFiniteTimestamp_ a = UnsafeMkSqlExpr ("ISFINITE(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @ISFINITE(timestamptz)@: test for a finite timestamptz (not +/-infinity).
isFiniteTimestamptz_ ::
  SqlExpr scope (SqlT n UTCTime) ->
  SqlExpr scope (SqlT n Bool)
isFiniteTimestamptz_ a = UnsafeMkSqlExpr ("ISFINITE(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @ISFINITE(interval)@: test for a finite interval (not +/-infinity).
isFiniteInterval_ ::
  SqlExpr scope (SqlT n DiffTime) ->
  SqlExpr scope (SqlT n Bool)
isFiniteInterval_ a = UnsafeMkSqlExpr ("ISFINITE(" <> unsafeGetSqlExpr a <> ")")
