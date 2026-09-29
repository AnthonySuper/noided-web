{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Select.FromClause where

import Control.Monad (when)
import GHC.Generics
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.Nullified
import Noided.Sql.Internal.Type.Nullability (Nullability (NonNull, Nullable))
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Tie

data OnClause baseSelectList joinedSelectList where
  JoinOn ::
    forall baseSelectList joinedSelectList nullability.
    ( baseSelectList (SqlExpr NormalQuery) ->
      joinedSelectList (SqlExpr NormalQuery) ->
      SqlExpr NormalQuery (SqlT nullability Bool)
    ) ->
    OnClause baseSelectList joinedSelectList

writeOnClause ::
  OnClause baseSelectList joinedSelectList ->
  baseSelectList (SqlExpr NormalQuery) ->
  joinedSelectList (SqlExpr NormalQuery) ->
  QueryWriter ()
writeOnClause (JoinOn f) lhs rhs = do
  " ON ("
  let res = f lhs rhs
  writeSyntax (unsafeGetSqlExpr res)
  writeSyntax ")"

-- | A FROM clause that results in a particular select list.
-- You should build these with helper functions like 'fromBase_', 'innerJoin_', and 'on_',
-- along with 'Data.Function.(&)' from 'Data.Function'.
-- Like so:
--
-- @
-- let fromClause = fromBase_ myTable & innerJoin_ otherTable `on_` \x y -> x.teacherId ==. y.id`
-- @
data FromClause selectList where
  FromBase ::
    (FromItem fromBase) =>
    fromBase ->
    FromClause (FromItemSelectList fromBase)
  InnerJoin ::
    (FromItem joinedItem) =>
    (QueriedRow baseFrom -> joinedItem) ->
    OnClause baseFrom (FromItemSelectList joinedItem) ->
    FromClause baseFrom ->
    FromClause (baseFrom :-: FromItemSelectList joinedItem)
  LeftJoin ::
    (FromItem joinedItem, Nullified (FromItemSelectList joinedItem)) =>
    (QueriedRow baseFrom -> joinedItem) ->
    OnClause baseFrom (FromItemSelectList joinedItem) ->
    FromClause baseFrom ->
    FromClause (baseFrom :-: AsNullified (FromItemSelectList joinedItem))
  RightJoin ::
    (FromItem joinedItem, Nullified baseFrom) =>
    (QueriedRow baseFrom -> joinedItem) ->
    OnClause baseFrom (FromItemSelectList joinedItem) ->
    FromClause baseFrom ->
    FromClause (AsNullified baseFrom :-: FromItemSelectList joinedItem)
  FullOuterJoin ::
    (FromItem joinedItem, Nullified baseFrom, Nullified (FromItemSelectList joinedItem)) =>
    (QueriedRow baseFrom -> joinedItem) ->
    OnClause baseFrom (FromItemSelectList joinedItem) ->
    FromClause baseFrom ->
    FromClause (AsNullified baseFrom :-: AsNullified (FromItemSelectList joinedItem))

data FromFirstItem = IsFirstFromItem | IsNotFirstFromItem
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

writeFromClause :: FromFirstItem -> FromClause selectList -> QueryWriter (QueriedRow selectList)
writeFromClause ff = \case
  FromBase base -> do
    when
      (ff == IsNotFirstFromItem && fromItemLateralUsage base == SometimesLateral)
      "LATERAL "
    writeFromItem base
    " AS "
    writeFromItemAfterAs base
  InnerJoin buildItem oc fromBase -> do
    selBase <- writeFromClause ff fromBase
    " INNER JOIN "
    let built = buildItem selBase
    when (fromItemLateralUsage built == SometimesLateral) "LATERAL "
    writeFromItem built
    " AS "
    joinedItem <- writeFromItemAfterAs built
    writeOnClause oc selBase joinedItem
    return $ selBase :-: joinedItem
  LeftJoin buildItem oc fromBase -> do
    selBase <- writeFromClause ff fromBase
    " LEFT JOIN "
    let built = buildItem selBase
    when (fromItemLateralUsage built == SometimesLateral) "LATERAL "
    writeFromItem built
    " AS "
    joinedItem <- writeFromItemAfterAs built
    writeOnClause oc selBase joinedItem
    return $ selBase :-: nullifyRow joinedItem
  RightJoin buildItem oc fromBase -> do
    selBase <- writeFromClause ff fromBase
    " RIGHT JOIN "
    let built = buildItem selBase
    when (fromItemLateralUsage built == SometimesLateral) "LATERAL "
    writeFromItem built
    " AS "
    joinedItem <- writeFromItemAfterAs built
    writeOnClause oc selBase joinedItem
    return $ nullifyRow selBase :-: joinedItem
  FullOuterJoin buildItem oc fromBase -> do
    selBase <- writeFromClause ff fromBase
    " FULL OUTER JOIN "
    let built = buildItem selBase
    when (fromItemLateralUsage built == SometimesLateral) "LATERAL "
    writeFromItem built
    " AS "
    joinedItem <- writeFromItemAfterAs built
    writeOnClause oc selBase joinedItem
    return $ nullifyRow selBase :-: nullifyRow joinedItem

fromBase_ ::
  (FromItem fromBase) =>
  fromBase ->
  FromClause (FromItemSelectList fromBase)
fromBase_ = FromBase

innerJoinLateral_ ::
  (FromItem joinedItem) =>
  (QueriedRow baseFrom -> joinedItem) ->
  OnClause baseFrom (FromItemSelectList joinedItem) ->
  FromClause baseFrom ->
  FromClause (baseFrom :-: FromItemSelectList joinedItem)
innerJoinLateral_ = InnerJoin

innerJoin_ ::
  (FromItem joinedItem) =>
  joinedItem ->
  OnClause baseFrom (FromItemSelectList joinedItem) ->
  FromClause baseFrom ->
  FromClause (baseFrom :-: FromItemSelectList joinedItem)
innerJoin_ c = innerJoinLateral_ (const c)

leftJoinLateral_ ::
  (FromItem joinedItem, Nullified (FromItemSelectList joinedItem)) =>
  (QueriedRow baseFrom -> joinedItem) ->
  OnClause baseFrom (FromItemSelectList joinedItem) ->
  FromClause baseFrom ->
  FromClause (baseFrom :-: AsNullified (FromItemSelectList joinedItem))
leftJoinLateral_ = LeftJoin

leftJoin_ ::
  (FromItem joinedItem, Nullified (FromItemSelectList joinedItem)) =>
  joinedItem ->
  OnClause baseFrom (FromItemSelectList joinedItem) ->
  FromClause baseFrom ->
  FromClause (baseFrom :-: AsNullified (FromItemSelectList joinedItem))
leftJoin_ c = leftJoinLateral_ (const c)

rightJoinLateral_ ::
  (FromItem joinedItem, Nullified baseFrom) =>
  (QueriedRow baseFrom -> joinedItem) ->
  OnClause baseFrom (FromItemSelectList joinedItem) ->
  FromClause baseFrom ->
  FromClause (AsNullified baseFrom :-: FromItemSelectList joinedItem)
rightJoinLateral_ = RightJoin

rightJoin_ ::
  (FromItem joinedItem, Nullified baseFrom) =>
  joinedItem ->
  OnClause baseFrom (FromItemSelectList joinedItem) ->
  FromClause baseFrom ->
  FromClause (AsNullified baseFrom :-: FromItemSelectList joinedItem)
rightJoin_ c = rightJoinLateral_ (const c)

fullOuterJoinLateral_ ::
  (FromItem joinedItem, Nullified baseFrom, Nullified (FromItemSelectList joinedItem)) =>
  (QueriedRow baseFrom -> joinedItem) ->
  OnClause baseFrom (FromItemSelectList joinedItem) ->
  FromClause baseFrom ->
  FromClause (AsNullified baseFrom :-: AsNullified (FromItemSelectList joinedItem))
fullOuterJoinLateral_ = FullOuterJoin

fullOuterJoin_ ::
  (FromItem joinedItem, Nullified baseFrom, Nullified (FromItemSelectList joinedItem)) =>
  joinedItem ->
  OnClause baseFrom (FromItemSelectList joinedItem) ->
  FromClause baseFrom ->
  FromClause (AsNullified baseFrom :-: AsNullified (FromItemSelectList joinedItem))
fullOuterJoin_ c = fullOuterJoinLateral_ (const c)

on_' ::
  forall nullability baseSelectList joinedSelectList t.
  (OnClause baseSelectList joinedSelectList -> t) ->
  ( baseSelectList (SqlExpr NormalQuery) ->
    joinedSelectList (SqlExpr NormalQuery) ->
    SqlExpr NormalQuery (SqlT nullability Bool)
  ) ->
  t
jc `on_'` f = jc (JoinOn f)

on_ ::
  (OnClause baseSelectList joinedSelectList -> t) ->
  ( baseSelectList (SqlExpr NormalQuery) ->
    joinedSelectList (SqlExpr NormalQuery) ->
    SqlExpr NormalQuery (SqlT NonNull Bool)
  ) ->
  t
on_ = on_' @NonNull

onNullable_ ::
  ( OnClause baseSelectList joinedSelectList -> t
  ) ->
  ( baseSelectList (SqlExpr NormalQuery) ->
    joinedSelectList (SqlExpr NormalQuery) ->
    SqlExpr NormalQuery (SqlT Nullable Bool)
  ) ->
  t
onNullable_ = on_' @Nullable
