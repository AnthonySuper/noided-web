module Noided.Sql.Internal.Type.Nullability where

import GHC.Generics

-- | Data kinda used to determine the nullability of an SQL value.
data Nullability = NonNull | Nullable
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

type MostNullable :: Nullability -> Nullability -> Nullability
type family MostNullable lhs rhs where
  MostNullable NonNull NonNull = NonNull
  MostNullable Nullable NonNull = Nullable
  MostNullable NonNull Nullable = Nullable
  MostNullable Nullable Nullable = Nullable
