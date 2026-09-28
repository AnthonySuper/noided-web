{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Noided.Sql.Internal.Class.DenullRow
-- Description: Recover a whole row from the nullable side of an outer join.
--
-- A @t 'Nulled@ row is decoded with every column nullable. 'denullRow' turns
-- it back into the declared @t 'NotNulled@ row:
--
-- * a column declared @NonNull@ that came back @NULL@ means the outer join
--   did not match, so the whole row is 'Nothing';
-- * a column declared @Nullable@ is passed through as-is.
--
-- This is only sound when the table has at least one @NonNull@ column; the
-- TH refuses to generate the unwrapping instance otherwise.
module Noided.Sql.Internal.Class.DenullRow
  ( DenullRow (..),
    GDenull,
  )
where

import Data.Kind (Type)
import GHC.Generics
import Noided.Sql.Internal.Type.Col
import Noided.Sql.Internal.Type.HaskellT
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType

class DenullRow (t :: NullTag -> (SqlType -> Type) -> Type) where
  denullRow :: t Nulled HaskellT -> Maybe (t NotNulled HaskellT)
  default denullRow ::
    ( Generic (t Nulled HaskellT),
      Generic (t NotNulled HaskellT),
      GDenull (Rep (t Nulled HaskellT)) (Rep (t NotNulled HaskellT))
    ) =>
    t Nulled HaskellT ->
    Maybe (t NotNulled HaskellT)
  denullRow = fmap to . gdenull . from

class GDenull i o where
  gdenull :: i p -> Maybe (o p)

instance (GDenull i o) => GDenull (M1 c m i) (M1 c m o) where
  gdenull (M1 a) = M1 <$> gdenull a

instance (GDenull i1 o1, GDenull i2 o2) => GDenull (i1 :*: i2) (o1 :*: o2) where
  gdenull (a :*: b) = (:*:) <$> gdenull a <*> gdenull b

instance GDenull U1 U1 where
  gdenull U1 = Just U1

-- | A column: its declared nullability decides whether NULL means "absent".
instance
  (KnownNullability n) =>
  GDenull (K1 R (HaskellT (SqlT Nullable t))) (K1 R (HaskellT (SqlT n t)))
  where
  gdenull (K1 (HaskT m)) =
    case nullabilityS @n of
      NonNullSing -> K1 . HaskT <$> m
      NullableSing -> Just (K1 (HaskT m))

-- | A nested table.
instance
  (DenullRow sub) =>
  GDenull (K1 R (sub Nulled HaskellT)) (K1 R (sub NotNulled HaskellT))
  where
  gdenull (K1 a) = K1 <$> denullRow a
