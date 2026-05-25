{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.SubAgg where

import Data.HKD
import Data.Proxy
import Noided.Sql.Internal.Class.PGType
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Select.AggregateQuery
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.SqlExpr.Bool
import Noided.Sql.Internal.SqlExpr.Row
import Noided.Sql.Internal.Type.AggregateExpr
import Noided.Sql.Internal.Type.PGArray
import Noided.Sql.Internal.Type.PGRow
import Noided.Sql.Internal.Type.QueryWriter (syntaxSubquery)
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

aggregatedSubqueryVal_ ::
  AggregateQuery EntireQuery (Element dt (SqlExpr Aggregated)) ->
  SqlExpr scope dt
aggregatedSubqueryVal_ aggQuery =
  UnsafeMkSqlExpr $
    "(" <> syntaxSubquery (writeQuerySyntax aggQuery) <> ")"

emptyRow :: forall i n. (PGType (PGArray i)) => SqlExpr n (NonNullT (PGArray i))
emptyRow =
  UnsafeMkSqlExpr $
    "ARRAY[]::" <> syntaxFromText typeName
  where
    typeName = pgTypeName (Proxy @(PGArray i))

subqueryRowsOf_ :: (FTraversable sl) => SelectM (QueriedRow sl) -> SqlExpr scope (NonNullT (PGArray (NonNullT (PGRow sl))))
subqueryRowsOf_ query = nullable `coalesce_` emptyRow
  where
    nullable =
      aggregatedSubqueryVal_ $
        aggregate_ (Element . agg . arrayAgg_ . row_) query
