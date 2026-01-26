{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Select.SelectMSpec (spec) where

import Data.HKD
import Data.Int (Int64)
import Data.Text (unpack)
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Select.FromClause
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.Syntax
import Noided.Sql.Internal.Type.Tie
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
  renderGolden "A one-element select, with no FROM or WHERE" $ do
    return $ Element (bindParam @Int64 10)
  renderGolden "Selecting two FROMs" $ do
    r1 <- addFrom_ $ fromBase_ (select_ $ Element $ bindParam @Int64 10)
    r2 <- addFrom_ $ fromBase_ (select_ $ Element $ bindParam @Int64 10)
    select_ $ r1 :-: r2
