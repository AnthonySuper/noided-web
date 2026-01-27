{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Sql.Internal.Select.Combine where

import Data.Kind
import GHC.Generics
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList (SelectList)
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlType

data CombineType = Union | Intersect | Except | UnionAll | IntersectAll | ExceptAll
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | Singleton type for query combine.
data CombineTypeSing (ct :: CombineType) where
  UnionSing :: CombineTypeSing Union
  IntersectSing :: CombineTypeSing Intersect
  ExceptSing :: CombineTypeSing Except
  UnionAllSing :: CombineTypeSing UnionAll
  IntersectAllSing :: CombineTypeSing IntersectAll
  ExceptAllSing :: CombineTypeSing ExceptAll

combineTypeVal :: CombineTypeSing ct -> CombineType
combineTypeVal UnionSing = Union
combineTypeVal IntersectSing = Intersect
combineTypeVal ExceptSing = Except
combineTypeVal UnionAllSing = UnionAll
combineTypeVal IntersectAllSing = IntersectAll
combineTypeVal ExceptAllSing = ExceptAll

-- | Singleton type for a known combine type.
class KnownCombineType (ct :: CombineType) where
  combineTypeS :: CombineTypeSing ct

instance KnownCombineType Union where
  combineTypeS = UnionSing

instance KnownCombineType Intersect where
  combineTypeS = IntersectSing

instance KnownCombineType Except where
  combineTypeS = ExceptSing

instance KnownCombineType UnionAll where
  combineTypeS = UnionAllSing

instance KnownCombineType IntersectAll where
  combineTypeS = IntersectAllSing

instance KnownCombineType ExceptAll where
  combineTypeS = ExceptAllSing

-- | Combine two queries, using @ UNION @, @ INTERSECT @, @ EXCEPT @.
data CombinedQueries selectList where
  CombineBase ::
    (SelectQuery query) => query -> CombinedQueries (QuerySelectList query)
  CombineTie ::
    (SelectQuery query, QuerySelectList query ~ selectList) =>
    query ->
    CombineType ->
    CombinedQueries selectList ->
    CombinedQueries selectList

appendCombined ::
  CombineType ->
  CombinedQueries selectList ->
  CombinedQueries selectList ->
  CombinedQueries selectList
appendCombined op (CombineBase q) rhs = CombineTie q op rhs
appendCombined op (CombineTie q op' rest) rhs = CombineTie q op' (appendCombined op rest rhs)

writeCombinedQueries :: CombinedQueries sl -> QueryWriter ()
writeCombinedQueries (CombineBase q) = writeQuerySyntax q
writeCombinedQueries (CombineTie q op rest) = do
  writeQuerySyntax q
  case op of
    Union -> " UNION "
    Intersect -> " INTERSECT "
    Except -> " EXCEPT "
    UnionAll -> " UNION ALL "
    IntersectAll -> " INTERSECT ALL "
    ExceptAll -> " EXCEPT ALL "
  writeCombinedQueries rest

instance (SelectList sl) => Query (CombinedQueries sl) where
  type QuerySelectList (CombinedQueries sl) = sl
  writeQuerySyntax = writeCombinedQueries

instance (SelectList sl) => SelectQuery (CombinedQueries sl)

instance
  (SelectList sl, UnwrapSelectList sl, DecodeSelectList sl) =>
  ExecutableQuery (CombinedQueries sl)

-- | Query combine of some type.
type QueryCombineOf :: CombineType -> ((SqlType -> Type) -> Type) -> Type
newtype QueryCombineOf combineType selectList = QueryCombineOf (CombinedQueries selectList)

instance (KnownCombineType combineType, SelectList selectList) => Semigroup (QueryCombineOf combineType selectList) where
  QueryCombineOf lhs <> QueryCombineOf rhs =
    QueryCombineOf $ appendCombined (combineTypeVal (combineTypeS @combineType)) lhs rhs

instance (KnownCombineType combineType, SelectList sl) => Query (QueryCombineOf combineType sl) where
  type QuerySelectList (QueryCombineOf combineType sl) = sl
  writeQuerySyntax (QueryCombineOf q) = writeCombinedQueries q

instance (KnownCombineType combineType, SelectList sl) => SelectQuery (QueryCombineOf combineType sl)

instance
  (KnownCombineType combineType, SelectList sl, UnwrapSelectList sl, DecodeSelectList sl) =>
  ExecutableQuery (QueryCombineOf combineType sl)

-- | Newtype wrapper to combine queries with union.
newtype QueryCombineUnion selectList = QueryCombineUnion (QueryCombineOf Union selectList)
  deriving newtype (Semigroup, Query, SelectQuery, ExecutableQuery)

newtype QueryCombineIntersect selectList = QueryCombineIntersect (QueryCombineOf Intersect selectList)
  deriving newtype (Semigroup, Query, SelectQuery, ExecutableQuery)

newtype QueryCombineExcept selectList = QueryCombineExcept (QueryCombineOf Except selectList)
  deriving newtype (Semigroup, Query, SelectQuery, ExecutableQuery)

newtype QueryCombineUnionAll selectList = QueryCombineUnionAll (QueryCombineOf UnionAll selectList)
  deriving newtype (Semigroup, Query, SelectQuery, ExecutableQuery)

newtype QueryCombineIntersectAll selectList = QueryCombineIntersectAll (QueryCombineOf IntersectAll selectList)
  deriving newtype (Semigroup, Query, SelectQuery, ExecutableQuery)

newtype QueryCombineExceptAll selectList = QueryCombineExceptAll (QueryCombineOf ExceptAll selectList)
  deriving newtype (Semigroup, Query, SelectQuery, ExecutableQuery)