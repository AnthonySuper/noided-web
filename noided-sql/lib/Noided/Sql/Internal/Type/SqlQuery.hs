{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Type.SqlQuery where

import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session (Session, script, statement)
import Hasql.Statement (Statement, unpreparable)
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Type.PGDecoder (decoderToColumn)
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.Syntax

-- | An opaque SQL query, returning rows of some type.
data SqlQuery decoded where
  UnsafeSqlQ ::
    { syntax :: Text,
      paramsInspected :: [Text],
      params :: Enc.Params (),
      decoder :: Dec.Result decoded
    } ->
    SqlQuery decoded
  ScriptSqlQ ::
    { scriptSyntax :: Text
    } ->
    SqlQuery ()

-- | Unsafely build a query from a script.
-- This should be used to do command-level functions, creating savepoints and the like.
unsafeQueryFromScript :: Text -> SqlQuery ()
unsafeQueryFromScript s = ScriptSqlQ {scriptSyntax = s}

sqlQueryToHasqlSession :: SqlQuery result -> Session result
sqlQueryToHasqlSession (UnsafeSqlQ s _ p d) = statement () $ unpreparable s p d
sqlQueryToHasqlSession (ScriptSqlQ s) = script s

sqlQuerySyntax :: SqlQuery decoded -> Text
sqlQuerySyntax (UnsafeSqlQ s _ _ _) = s
sqlQuerySyntax (ScriptSqlQ s) = s

sqlQueryInspectedBinds :: SqlQuery decoded -> [Text]
sqlQueryInspectedBinds (UnsafeSqlQ _ i _ _) = i
sqlQueryInspectedBinds (ScriptSqlQ _) = []

buildQueryFromRow ::
  (ExecutableQuery query) =>
  (Dec.Row (QueryResult query) -> Dec.Result b) ->
  query ->
  SqlQuery b
buildQueryFromRow mapper q =
  UnsafeSqlQ
    { syntax = queryT,
      paramsInspected = inspectedBinds,
      params = params,
      decoder = mapper usedDecoder
    }
  where
    usedDecoder = decodeQueryResult q decoderToColumn
    (queryT, params, inspectedBinds) = renderSyntaxToTextWithBinds $ renderQueryWriter $ writeQuerySyntax q

sqlQuerySingleRow :: (ExecutableQuery query) => query -> SqlQuery (QueryResult query)
sqlQuerySingleRow = buildQueryFromRow Dec.singleRow

sqlQueryVector :: (ExecutableQuery query) => query -> SqlQuery (Vector (QueryResult query))
sqlQueryVector = buildQueryFromRow Dec.rowVector

sqlQueryMaybe :: (ExecutableQuery query) => query -> SqlQuery (Maybe (QueryResult query))
sqlQueryMaybe = buildQueryFromRow Dec.rowMaybe

sqlQueryList :: (ExecutableQuery query) => query -> SqlQuery [QueryResult query]
sqlQueryList = buildQueryFromRow Dec.rowList

sqlQueryFoldl :: (ExecutableQuery query) => (b -> QueryResult query -> b) -> b -> query -> SqlQuery b
sqlQueryFoldl f acc = buildQueryFromRow $ Dec.foldlRows f acc

sqlQueryFoldr :: (ExecutableQuery query) => (QueryResult query -> b -> b) -> b -> query -> SqlQuery b
sqlQueryFoldr f acc = buildQueryFromRow $ Dec.foldrRows f acc
