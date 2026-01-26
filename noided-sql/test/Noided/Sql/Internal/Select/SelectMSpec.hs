{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Select.SelectMSpec (spec) where

import Data.Function ((&))
import Data.HKD
import Data.Int (Int64)
import Data.Text (unpack)
import GHC.Generics
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Select.FromClause
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.SqlExpr.Bool ((==.))
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Noided.Sql.Internal.Type.Tie
import Test.Hspec
import Test.Hspec.Golden

data Table1 f = Table1 {t1Id :: f (NonNullT Int64), t1Val :: f (NonNullT Int64)} deriving (Generic)

instance FFunctor Table1 where ffmap = ffmapDefault

instance FFoldable Table1 where ffoldMap = ffoldMapDefault

instance FTraversable Table1 where ftraverse = gftraverse

instance FZip Table1 where fzipWith = gfzipWith

instance NamedColumns Table1 where
  namedColumns = Table1 "id" "val"

data Table2 f = Table2 {t2Id :: f (NonNullT Int64), t2Ref :: f (NonNullT Int64)} deriving (Generic)

instance FFunctor Table2 where ffmap = ffmapDefault

instance FFoldable Table2 where ffoldMap = ffoldMapDefault

instance FTraversable Table2 where ftraverse = gftraverse

instance FZip Table2 where fzipWith = gfzipWith

instance NamedColumns Table2 where
  namedColumns = Table2 "id" "ref"

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

  renderGolden "Selecting with an INNER JOIN" $ do
    let subq1 = select_ (Table1 (bindParam @Int64 1) (bindParam @Int64 2))
    let subq2 = select_ (Table2 (bindParam @Int64 3) (bindParam @Int64 4))
    r <- addFrom_ $ fromBase_ subq1 & innerJoin_ subq2 `on_` (\t1 t2 -> t1Id t1 ==. t2Ref t2)
    select_ r
