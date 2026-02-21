{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.PGSeriesSpec (spec) where

import Data.Function ((&))
import Data.HKD
import Data.Int (Int64)
import Data.Text (unpack)
import Data.Time (DiffTime, LocalTime, UTCTime)
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
    r <- addFrom_ $ fromBase_ (generateSeries (bindParam @Int64 1) (bindParam @Int64 10))
    return r

  renderGolden "generate_series with start, stop, and step" $ do
    r <- addFrom_ $ fromBase_ (generateSeriesStep (bindParam @Int64 0) (bindParam @Int64 100) (bindParam @Int64 5))
    return r

  renderGolden "generate_series in a lateral join" $ do
    let series1 = generateSeries (bindParam @Int64 1) (bindParam @Int64 5)
    let series2 = generateSeries (bindParam @Int64 6) (bindParam @Int64 10)
    r <- addFrom_ $
      fromBase_ series1
        & innerJoin_ series2
        `on_` (\_ _ -> bindParam True)
    return r

  renderGolden "generate_series with UTCTime and interval step" $ do
    r <-
      addFrom_ $
        fromBase_ $
          generateSeriesStep
            (bindParam @UTCTime (read "2024-01-01 00:00:00 UTC"))
            (bindParam @UTCTime (read "2024-12-31 00:00:00 UTC"))
            (bindParam @DiffTime 86400)
    return r

  renderGolden "generate_series with LocalTime and interval step" $ do
    r <-
      addFrom_ $
        fromBase_ $
          generateSeriesStep
            (bindParam @LocalTime (read "2024-01-01 00:00:00"))
            (bindParam @LocalTime (read "2024-12-31 00:00:00"))
            (bindParam @DiffTime 86400)
    return r

