{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Sql.Internal.Class.Nullified where

import Data.HKD
import Data.Kind (Type)
import GHC.Generics
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Type.Nullability (Nullability (Nullable))
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Tie

-- | Typeclass for select lists that can be "nullified" (e.g. for outer joins).
class (SelectList sl, SelectList (AsNullified sl)) => Nullified (sl :: (SqlType -> Type) -> Type) where
  type AsNullified sl :: (SqlType -> Type) -> Type
  nullifyRow :: sl (SqlExpr scope) -> AsNullified sl (SqlExpr scope)

  default nullifyRow ::
    ( Generic (sl (SqlExpr scope)),
      Generic (AsNullified sl (SqlExpr scope)),
      GNullified (Rep (sl (SqlExpr scope))) (Rep (AsNullified sl (SqlExpr scope)))
    ) =>
    sl (SqlExpr scope) ->
    AsNullified sl (SqlExpr scope)
  nullifyRow = to . gNullifyRow . from

-- | Generic implementation of nullification.
class GNullified i o where
  gNullifyRow :: i p -> o p

instance (GNullified i o) => GNullified (M1 c d i) (M1 c d o) where
  gNullifyRow (M1 a) = M1 (gNullifyRow a)

instance (GNullified i1 o1, GNullified i2 o2) => GNullified (i1 :*: i2) (o1 :*: o2) where
  gNullifyRow (a :*: b) = gNullifyRow a :*: gNullifyRow b

instance GNullified U1 U1 where
  gNullifyRow U1 = U1

instance GNullified (K1 R (SqlExpr scope (SqlT n a))) (K1 R (SqlExpr scope (SqlT Nullable a))) where
  gNullifyRow (K1 (UnsafeMkSqlExpr e)) = K1 (UnsafeMkSqlExpr e)

instance (Nullified sl, f ~ SqlExpr scope, sl' ~ AsNullified sl) => GNullified (K1 R (sl f)) (K1 R (sl' f)) where
  gNullifyRow (K1 a) = K1 (nullifyRow a)

instance Nullified (Element (SqlT n a)) where
  type AsNullified (Element (SqlT n a)) = Element (SqlT Nullable a)
  nullifyRow (Element e) = Element (UnsafeMkSqlExpr (unsafeGetSqlExpr e))

instance (Nullified l, Nullified r) => Nullified (l :-: r) where
  type AsNullified (l :-: r) = AsNullified l :-: AsNullified r
  nullifyRow (l :-: r) = nullifyRow l :-: nullifyRow r

instance (Nullified l, Nullified r) => Nullified (l :*: r) where
  type AsNullified (l :*: r) = AsNullified l :*: AsNullified r
  nullifyRow (l :*: r) = nullifyRow l :*: nullifyRow r
