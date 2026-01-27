{-# LANGUAGE DerivingStrategies #-}

module Noided.Sql.Internal.Select.Combine where

import Data.Kind
import GHC.Generics
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList (SelectList)
import Noided.Sql.Internal.Type.SqlType

data CombineType = Union | Intersect | Except | UnionAll | IntersectAll | ExceptAll
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | Singleton type for query combine.
--
-- TODO: add other cases as needed
data CombineTypeSing (ct :: CombineType) where
  UnionSing :: CombineTypeSing Union
  IntersectSing :: CombineTypeSing Intersect
  ExceptSing :: CombineTypeSing Except

-- | Singleton type for a known combine type.
--
-- TODO: implement instances.
class KnownCombineType (ct :: CombineType) where
  combineTypeS :: CombineTypeSing ct

instance KnownCombineType Union where
  combineTypeS = UnionSing

-- | Combine two queries, using @ UNION @, @ INTERSECT @, @ EXCEPT @.
-- TODO: implement 'AsQuery', 'AsSelectQuery', 'ExecutableQuery' for this type.
data CombinedQueries selectList where
  CombineBase ::
    (SelectQuery query) => query -> CombinedQueries (QuerySelectList query)
  CombineTie ::
    (SelectQuery query, QuerySelectList query ~ selectList) =>
    query ->
    CombineType ->
    CombinedQueries selectList ->
    CombinedQueries selectList

-- | Query combine of some type.
-- TODO: implement 'AsQuery', 'AsSelectQuery', 'ExecutableQuery' for this type.
type QueryCombineOf :: CombineType -> ((SqlType -> Type) -> Type) -> Type
newtype QueryCombineOf combineType selectList = QueryCombineOf (CombinedQueries selectList)

instance (KnownCombineType combineType, SelectList selectList) => Semigroup (QueryCombineOf combineType selectList) where
  (<>) = error "implement me"

-- | Newtype wrapper to combine queries with union.
-- TODO: implement 'AsQuery', 'AsSelectQuery', 'ExeceutableQuery' for this type.
-- TODO: implement similar newtype wrappers for all other combine types.
newtype QueryCombineUnion selectList = QueryCombineUnion (QueryCombineOf Union selectList)
  deriving newtype (Semigroup)
