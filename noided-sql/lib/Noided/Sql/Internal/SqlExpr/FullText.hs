{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.FullText where

import Data.Text (Text)
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.PGFullTextSearchWeight
import Noided.Sql.Internal.Type.PGRegConfig
import Noided.Sql.Internal.Type.PGTSQuery
import Noided.Sql.Internal.Type.PGTSVector
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

infix 4 @@.

-- | Full-text search match operator @@@@.
(@@.) ::
  SqlExpr scope (SqlT n PGTSVector) ->
  SqlExpr scope (SqlT n' PGTSQuery) ->
  SqlExpr scope (SqlT (MostNullable n n') Bool)
a @@. b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") @@ (" <> unsafeGetSqlExpr b <> ")")

-- | tsvector concatenation operator @||@.
concatTSVector_ ::
  SqlExpr scope (SqlT n PGTSVector) ->
  SqlExpr scope (SqlT n' PGTSVector) ->
  SqlExpr scope (SqlT (MostNullable n n') PGTSVector)
concatTSVector_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") || (" <> unsafeGetSqlExpr b <> ")")

-- | tsquery AND operator @&@.
tsAnd_ ::
  SqlExpr scope (SqlT n PGTSQuery) ->
  SqlExpr scope (SqlT n' PGTSQuery) ->
  SqlExpr scope (SqlT (MostNullable n n') PGTSQuery)
tsAnd_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") & (" <> unsafeGetSqlExpr b <> ")")

-- | tsquery OR operator @|@.
tsOr_ ::
  SqlExpr scope (SqlT n PGTSQuery) ->
  SqlExpr scope (SqlT n' PGTSQuery) ->
  SqlExpr scope (SqlT (MostNullable n n') PGTSQuery)
tsOr_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") | (" <> unsafeGetSqlExpr b <> ")")

-- | tsquery NOT operator @!!@.
tsNot_ :: SqlExpr scope (SqlT n PGTSQuery) -> SqlExpr scope (SqlT n PGTSQuery)
tsNot_ a = UnsafeMkSqlExpr ("!! (" <> unsafeGetSqlExpr a <> ")")

-- | Sql @to_tsvector@ function.
toTSVector_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n PGTSVector)
toTSVector_ a = UnsafeMkSqlExpr ("to_tsvector(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @to_tsvector@ function with config.
toTSVectorWithConfig_ ::
  SqlExpr scope (SqlT n PGRegConfig) ->
  SqlExpr scope (SqlT n' Text) ->
  SqlExpr scope (SqlT (MostNullable n n') PGTSVector)
toTSVectorWithConfig_ c a =
  UnsafeMkSqlExpr ("to_tsvector(" <> unsafeGetSqlExpr c <> ", " <> unsafeGetSqlExpr a <> ")")

-- | Sql @to_tsquery@ function.
toTSQuery_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n PGTSQuery)
toTSQuery_ a = UnsafeMkSqlExpr ("to_tsquery(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @to_tsquery@ function with config.
toTSQueryWithConfig_ ::
  SqlExpr scope (SqlT n PGRegConfig) ->
  SqlExpr scope (SqlT n' Text) ->
  SqlExpr scope (SqlT (MostNullable n n') PGTSQuery)
toTSQueryWithConfig_ c a =
  UnsafeMkSqlExpr ("to_tsquery(" <> unsafeGetSqlExpr c <> ", " <> unsafeGetSqlExpr a <> ")")

-- | Sql @plainto_tsquery@ function.
plainToTSQuery_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n PGTSQuery)
plainToTSQuery_ a = UnsafeMkSqlExpr ("plainto_tsquery(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @plainto_tsquery@ function with config.
plainToTSQueryWithConfig_ ::
  SqlExpr scope (SqlT n PGRegConfig) ->
  SqlExpr scope (SqlT n' Text) ->
  SqlExpr scope (SqlT (MostNullable n n') PGTSQuery)
plainToTSQueryWithConfig_ c a =
  UnsafeMkSqlExpr ("plainto_tsquery(" <> unsafeGetSqlExpr c <> ", " <> unsafeGetSqlExpr a <> ")")

-- | Sql @phraseto_tsquery@ function.
phraseToTSQuery_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n PGTSQuery)
phraseToTSQuery_ a = UnsafeMkSqlExpr ("phraseto_tsquery(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @phraseto_tsquery@ function with config.
phraseToTSQueryWithConfig_ ::
  SqlExpr scope (SqlT n PGRegConfig) ->
  SqlExpr scope (SqlT n' Text) ->
  SqlExpr scope (SqlT (MostNullable n n') PGTSQuery)
phraseToTSQueryWithConfig_ c a =
  UnsafeMkSqlExpr ("phraseto_tsquery(" <> unsafeGetSqlExpr c <> ", " <> unsafeGetSqlExpr a <> ")")

-- | Sql @websearch_to_tsquery@ function.
websearchToTSQuery_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n PGTSQuery)
websearchToTSQuery_ a = UnsafeMkSqlExpr ("websearch_to_tsquery(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @websearch_to_tsquery@ function with config.
websearchToTSQueryWithConfig_ ::
  SqlExpr scope (SqlT n PGRegConfig) ->
  SqlExpr scope (SqlT n' Text) ->
  SqlExpr scope (SqlT (MostNullable n n') PGTSQuery)
websearchToTSQueryWithConfig_ c a =
  UnsafeMkSqlExpr ("websearch_to_tsquery(" <> unsafeGetSqlExpr c <> ", " <> unsafeGetSqlExpr a <> ")")

-- | Sql @setweight@ function.
setWeight_ ::
  SqlExpr scope (SqlT n PGTSVector) ->
  PGFullTextSearchWeight ->
  SqlExpr scope (SqlT n PGTSVector)
setWeight_ v w =
  UnsafeMkSqlExpr ("setweight(" <> unsafeGetSqlExpr v <> ", '" <> pgFullTextSearchWeightSyntax w <> "')")

-- | Sql @ts_rank@ function.
tsRank_ ::
  SqlExpr scope (SqlT n PGTSVector) ->
  SqlExpr scope (SqlT n' PGTSQuery) ->
  SqlExpr scope (SqlT (MostNullable n n') Float)
tsRank_ v q =
  UnsafeMkSqlExpr ("ts_rank(" <> unsafeGetSqlExpr v <> ", " <> unsafeGetSqlExpr q <> ")")

-- | Sql @ts_rank_cd@ function.
tsRankCd_ ::
  SqlExpr scope (SqlT n PGTSVector) ->
  SqlExpr scope (SqlT n' PGTSQuery) ->
  SqlExpr scope (SqlT (MostNullable n n') Float)
tsRankCd_ v q =
  UnsafeMkSqlExpr ("ts_rank_cd(" <> unsafeGetSqlExpr v <> ", " <> unsafeGetSqlExpr q <> ")")
