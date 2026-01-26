{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Select.SelectM where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable (for_)
import Data.Functor
import Data.HKD
import Data.Sequence qualified as Seq
import GHC.Generics
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Select.FromClause
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

data SelectMState
  = SelectMState
  { fromSyntaxes :: Seq.Seq Syntax,
    whereSyntaxes :: Seq.Seq Syntax
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (Generically SelectMState)

writeAnds :: Seq.Seq Syntax -> Maybe (QueryWriter ())
writeAnds (a Seq.:<| Seq.Empty) = Just (writeSyntax $ "(" <> a <> ")")
writeAnds (a Seq.:<| b) =
  (writeSyntax ("(" <> a <> ") AND ") *>)
    <$> writeAnds b
writeAnds Seq.Empty = Nothing

newtype SelectM a
  = UnsafeMkSelectM
  {unsafeGetSelectM :: StateT SelectMState QueryWriter a}
  deriving newtype (Functor, Applicative, Monad)

addWhere_ :: SqlExpr NormalQuery (SqlT n Bool) -> SelectM ()
addWhere_ (UnsafeMkSqlExpr expr) = UnsafeMkSelectM $ modify $ \x ->
  x {whereSyntaxes = whereSyntaxes x Seq.:|> expr}

addFrom_ :: FromClause selectList -> SelectM (QueriedRow selectList)
addFrom_ fi = UnsafeMkSelectM $ do
  allSyns <- fromSyntaxes <$> get
  let ff = if Seq.null allSyns then IsFirstFromItem else IsNotFirstFromItem
  (res, syn) <- lift (delayWriting $ writeFromClause ff fi)
  modify $ \x -> x {fromSyntaxes = fromSyntaxes x Seq.:|> syn}
  return res

select_ :: (FZip t, FTraversable t, NamedColumns t) => t (SqlExpr NormalQuery) -> SelectM (t (SqlExpr NormalQuery))
select_ = return

renderSelectM :: (FZip t, FTraversable t, NamedColumns t) => SelectM (t (SqlExpr scope)) -> QueryWriter (t (SqlExpr scope))
renderSelectM a = do
  (result, finalState) <- runStateT (unsafeGetSelectM a) mempty
  "SELECT"
  for_ (aliasedColumnList result) $ \res -> do
    " "
    writeSyntax res
  for_ (fromCommaSepWritten $ foldMap Written finalState.fromSyntaxes) $ \syns -> do
    " FROM "
    writeSyntax syns
  for_ (writeAnds finalState.whereSyntaxes) $ \act -> do
    " WHERE "
    act
  return result

instance
  (wrapper ~ SqlExpr NormalQuery, FZip sl, FTraversable sl, NamedColumns sl) =>
  FromItem (SelectM (sl wrapper))
  where
  type FromItemSelectList (SelectM (sl wrapper)) = sl
  fromItemLateralUsage _ = SometimesLateral
  writeFromItem a = do
    "("
    renderSelectM a
    ")"
  fromItemAlias _ = "subq"
  fromItemSelectList _ = namedColumns
