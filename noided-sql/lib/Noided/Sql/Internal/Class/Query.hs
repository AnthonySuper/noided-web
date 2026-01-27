{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Sql.Internal.Class.Query where

import Data.HKD
import Data.Kind (Type)
import Noided.Sql.Internal.Class.DecodeSelectList (DecodeSelectList (selectListDecoder))
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Type.HaskellT
import Noided.Sql.Internal.Type.PGDecoder
import Noided.Sql.Internal.Type.QueryWriter (QueryWriter)
import Noided.Sql.Internal.Type.SqlType

-- | Queries that can be executed to obtain some result.
class (SelectList (QuerySelectList query)) => Query query where
  -- | The select list of a given query.
  -- Used with sub-selects and other things of that nature.
  type QuerySelectList query :: ((SqlType -> Type) -> Type)

  -- | Write out the full syntax of the query.
  writeQuerySyntax :: query -> QueryWriter ()

-- | If there is an instance of this class, the query is a SELECT query, and can be used in EXISTS and friends.
class (Query query) => SelectQuery query

class (Query query) => ExecutableQuery query where
  -- | The result of decoding this query.
  type QueryResult query :: Type

  type QueryResult query = SelectListUnwrapped (QuerySelectList query)
  decodeQueryResult ::
    forall f.
    (Applicative f) =>
    query ->
    (forall sqlT. PGDecoder sqlT -> f (HaskellT sqlT)) ->
    f (QueryResult query)
  default decodeQueryResult ::
    forall f.
    ( Applicative f,
      QueryResult query ~ SelectListUnwrapped (QuerySelectList query),
      UnwrapSelectList (QuerySelectList query),
      DecodeSelectList (QuerySelectList query)
    ) =>
    query ->
    (forall sqlT. PGDecoder sqlT -> f (HaskellT sqlT)) ->
    f (QueryResult query)
  decodeQueryResult _ f =
    unwrapSelectList
      <$> ftraverse f (selectListDecoder @(QuerySelectList query))
