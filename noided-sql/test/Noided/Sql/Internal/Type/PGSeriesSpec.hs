{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.PGSeriesSpec (spec) where

import Data.Function ((&))
import Data.HKD
import Data.Int (Int64)
import Data.Text (unpack)
import Data.Time (LocalTime, UTCTime)
import Noided.Sql.Internal.Type.Interval
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Select.FromClause
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.Type.PGSeries
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec
import Test.Hspec.Golden

renderGolden ::
  (FZip t, FTraversable t, NamedColumns t) =>
  String ->
  SelectM (t (SqlExpr NormalQuery)) ->
  Spec
renderGolden description selectM =
  golden description (return syntaxString)
  where
    syntaxString = unpack (renderSyntaxToTextNumberedBinds syntax)
    syntax = renderQueryWriter (renderSelectM selectM)

spec :: Spec
spec = do
  renderGolden "generate_series with start and stop" $ do
    r <- addFrom_ $ fromBase_ (generateSeries_ (bindParam_ @Int64 1) (bindParam_ @Int64 10))
    return r

  renderGolden "generate_series with start, stop, and step" $ do
    r <- addFrom_ $ fromBase_ (generateSeriesStep_ (bindParam_ @Int64 0) (bindParam_ @Int64 100) (bindParam_ @Int64 5))
    return r

  renderGolden "generate_series in a lateral join" $ do
    let series1 = generateSeries_ (bindParam_ @Int64 1) (bindParam_ @Int64 5)
    let series2 = generateSeries_ (bindParam_ @Int64 6) (bindParam_ @Int64 10)
    r <- addFrom_ $
      fromBase_ series1
        & innerJoin_ series2
        `on_` (\_ _ -> bindParam_ True)
    return r

  renderGolden "generate_series with UTCTime and interval step" $ do
    r <-
      addFrom_ $
        fromBase_ $
          generateSeriesStep_
            (bindParam_ @UTCTime (read "2024-01-01 00:00:00 UTC"))
            (bindParam_ @UTCTime (read "2024-12-31 00:00:00 UTC"))
            (bindParam_ @Interval (intervalFromDiffTime 86400))
    return r

  renderGolden "generate_series with LocalTime and interval step" $ do
    r <-
      addFrom_ $
        fromBase_ $
          generateSeriesStep_
            (bindParam_ @LocalTime (read "2024-01-01 00:00:00"))
            (bindParam_ @LocalTime (read "2024-12-31 00:00:00"))
            (bindParam_ @Interval (intervalFromDiffTime 86400))
    return r

