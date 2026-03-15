{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.OrderClause where

import Data.List.NonEmpty qualified as NE
import GHC.Generics
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.Syntax (CommaSepSyntax (..), Syntax, fromCommaSepSyntax)

data NullsOrdering = NullsFirst | NullsLast
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

data SqlOrderDirection = OrderAsc | OrderDesc
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

data SqlOrder = MkSqlOrder {orderDir :: SqlOrderDirection, orderNulls :: NullsOrdering}
  deriving (Generic)

orderAsc :: SqlOrder
orderAsc = MkSqlOrder {orderDir = OrderAsc, orderNulls = NullsLast}

orderDesc :: SqlOrder
orderDesc = MkSqlOrder {orderDir = OrderDesc, orderNulls = NullsFirst}

orderAscNullsFirst :: SqlOrder
orderAscNullsFirst = MkSqlOrder {orderDir = OrderAsc, orderNulls = NullsFirst}

orderAscNullsLast :: SqlOrder
orderAscNullsLast = orderAsc

orderDescNullsFirst :: SqlOrder
orderDescNullsFirst = orderDesc

orderDescNullsLast :: SqlOrder
orderDescNullsLast = MkSqlOrder {orderDir = OrderDesc, orderNulls = NullsLast}

data OrderItem scope where
  SomeOrderItem ::
    SqlExpr scope dt ->
    SqlOrder ->
    OrderItem scope

unsafeOrderItemToSyntax ::
  OrderItem scope -> Syntax
unsafeOrderItemToSyntax (SomeOrderItem expr order) =
  unsafeGetSqlExpr expr <> " " <> sqlOrderToSyntax order

sqlOrderToSyntax :: SqlOrder -> Syntax
sqlOrderToSyntax (MkSqlOrder dir nulls) =
  dirToSyntax dir <> " " <> nullsToSyntax nulls

dirToSyntax :: SqlOrderDirection -> Syntax
dirToSyntax OrderAsc = "ASC"
dirToSyntax OrderDesc = "DESC"

nullsToSyntax :: NullsOrdering -> Syntax
nullsToSyntax NullsFirst = "NULLS FIRST"
nullsToSyntax NullsLast = "NULLS LAST"

newtype OrderClause scope = MkOrderClause {getOrderClause :: NE.NonEmpty (OrderItem scope)}
  deriving newtype (Semigroup)

unsafeOrderClauseToSyntax ::
  OrderClause scope ->
  Syntax
unsafeOrderClauseToSyntax (MkOrderClause items) =
  fromCommaSepSyntax $ foldMap (Written . unsafeOrderItemToSyntax) items

orderingBy_ :: SqlOrder -> SqlExpr scope dt -> OrderClause scope
orderingBy_ oc syn = MkOrderClause $ pure $ SomeOrderItem syn oc

orderBy_ :: SqlExpr scope dt -> SqlOrder -> OrderClause scope
orderBy_ = flip orderingBy_

asc_ :: SqlExpr scope dt -> OrderClause scope
asc_ = orderingBy_ orderAsc

desc_ :: SqlExpr scope dt -> OrderClause scope
desc_ = orderingBy_ orderDesc

ascNullFirst_ :: SqlExpr scope dt -> OrderClause scope
ascNullFirst_ = orderingBy_ orderAscNullsFirst

ascNullsLast_ :: SqlExpr scope dt -> OrderClause scope
ascNullsLast_ = orderingBy_ orderAscNullsLast

descNullsFirst_ :: SqlExpr scope dt -> OrderClause scope
descNullsFirst_ = orderingBy_ orderDescNullsFirst

descNullsLast_ :: SqlExpr scope dt -> OrderClause scope
descNullsLast_ = orderingBy_ orderDescNullsLast
