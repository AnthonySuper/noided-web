{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Type.SqlQuery where

import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement, unpreparable)
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Type.PGDecoder (decoderToColumn)
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.Syntax

-- | An opaque SQL query, returning rows of some type.
data SqlQuery decoded
  = UnsafeSqlQ
  { syntax :: Text,
    paramsInspected :: [Text],
    params :: Enc.Params (),
    decoder :: Dec.Result decoded
  }

-- | Unsafely build a query from a script.
-- This should be used to do command-level functions, creating savepoints and the like.
unsafeQueryFromScript :: Text -> SqlQuery ()
unsafeQueryFromScript script =
  UnsafeSqlQ {syntax = script, paramsInspected = [], params = mempty, decoder = Dec.noResult}

sqlQueryToHasqlStatement :: SqlQuery decoded -> Statement () decoded
sqlQueryToHasqlStatement sql = unpreparable sql.syntax sql.params sql.decoder

sqlQueryToHasqlSession :: SqlQuery result -> Session result
sqlQueryToHasqlSession = statement () . sqlQueryToHasqlStatement

sqlQuerySyntax :: SqlQuery decoded -> Text
sqlQuerySyntax = syntax

sqlQueryInspectedBinds :: SqlQuery decoded -> [Text]
sqlQueryInspectedBinds = paramsInspected

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
