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
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
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

-- | Builds the FROM (or USING) items of an UPDATE or DELETE.
-- Postgres does not allow these to reference the target row, so it is not available here.
-- Only 'addFromItem_' is allowed; conditions belong in 'WhereM'.
newtype FromM a = UnsafeMkFromM {unsafeGetFromM :: SelectM a}
  deriving newtype (Functor, Applicative, Monad)

-- | Builds the WHERE conditions of an UPDATE or DELETE, which may reference the target row and the FROM items.
-- Only 'addWhereCondition_' is allowed.
newtype WhereM a = UnsafeMkWhereM {unsafeGetWhereM :: SelectM a}
  deriving newtype (Functor, Applicative, Monad)

-- | Like 'addFrom_', but for the FROM/USING items of an UPDATE or DELETE.
addFromItem_ :: FromClause selectList -> FromM (QueriedRow selectList)
addFromItem_ = UnsafeMkFromM . addFrom_

-- | Like 'addWhere_', but for the WHERE clause of an UPDATE or DELETE.
addWhereCondition_ :: SqlExpr NormalQuery (SqlT n Bool) -> WhereM ()
addWhereCondition_ = UnsafeMkWhereM . addWhere_

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
  (wrapper ~ SqlExpr NormalQuery, SelectList sl) =>
  FromItem (SelectM (sl wrapper))
  where
  type FromItemSelectList (SelectM (sl wrapper)) = sl
  fromItemLateralUsage _ = SometimesLateral
  writeFromItem a = do
    "("
    _ <- renderSelectM a
    ")"
  fromItemAlias _ = "subq"
  fromItemSelectList _ = namedColumns

instance
  (wrapper ~ SqlExpr NormalQuery, SelectList selectList) =>
  Query (SelectM (selectList wrapper))
  where
  type QuerySelectList (SelectM (selectList wrapper)) = selectList
  writeQuerySyntax = void . renderSelectM

instance
  (wrapper ~ SqlExpr NormalQuery, SelectList selectList) =>
  SelectQuery (SelectM (selectList wrapper))

instance
  ( wrapper ~ SqlExpr NormalQuery,
    SelectList selectList,
    UnwrapSelectList selectList,
    DecodeSelectList selectList
  ) =>
  ExecutableQuery (SelectM (selectList wrapper))
