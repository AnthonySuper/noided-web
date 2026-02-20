module Noided.Sql.Internal.Type.Nullability where

import GHC.Generics
import Data.Typeable

-- | Data kind used to determine the nullability of an SQL value.
data Nullability = NonNull | Nullable
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic, Typeable)

-- | Singleton type for nullabililty
data NullabilitySing nullability where
  NonNullSing :: NullabilitySing NonNull
  NullableSing :: NullabilitySing Nullable

-- | Singleton class for nullability
class (Typeable nullability) => KnownNullability nullability where
  nullabilityS :: NullabilitySing nullability

instance KnownNullability NonNull where
  nullabilityS = NonNullSing

instance KnownNullability Nullable where
  nullabilityS = NullableSing

type MostNullable :: Nullability -> Nullability -> Nullability
type family MostNullable lhs rhs where
  MostNullable NonNull NonNull = NonNull
  MostNullable Nullable NonNull = Nullable
  MostNullable NonNull Nullable = Nullable
  MostNullable Nullable Nullable = Nullable
