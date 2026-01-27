{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Sql.Internal.Class.DecodeSelectList where

import Data.HKD
import GHC.Generics
import Noided.Row
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.PGDecoder
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Tie

-- | Select lists that can be *decoded*.
class DecodeSelectList hkd where
  selectListDecoder :: hkd PGDecoder
  default selectListDecoder :: (Generic (hkd PGDecoder), GDecodeSelectList (Rep (hkd PGDecoder))) => hkd PGDecoder
  selectListDecoder = gselectListDecoder

instance (KnownNullability nullability, AsHaskellValue pgType) => DecodeSelectList (Element (SqlT nullability pgType)) where
  selectListDecoder = Element pgDecoder

instance (DecodeSelectList lhs, DecodeSelectList rhs) => DecodeSelectList (lhs :*: rhs) where
  selectListDecoder = selectListDecoder :*: selectListDecoder

instance (DecodeSelectList lhs, DecodeSelectList rhs) => DecodeSelectList (lhs :-: rhs) where
  selectListDecoder = selectListDecoder :-: selectListDecoder

instance DecodeSelectList (WrappedRow '[]) where
  selectListDecoder = EmptyWrappedRow

instance
  ( t ~ SqlT nullability pgType,
    KnownNullability nullability,
    AsHaskellValue pgType,
    DecodeSelectList (WrappedRow rest)
  ) =>
  DecodeSelectList (WrappedRow (l :=> t ': rest))
  where
  selectListDecoder = pgDecoder :::% selectListDecoder

class GDecodeSelectList rep where
  genericRowDecoder :: rep ()

instance (GDecodeSelectList f) => GDecodeSelectList (M1 i c f) where
  genericRowDecoder = M1 genericRowDecoder

instance (GDecodeSelectList l, GDecodeSelectList r) => GDecodeSelectList (l :*: r) where
  genericRowDecoder = genericRowDecoder :*: genericRowDecoder

instance GDecodeSelectList U1 where
  genericRowDecoder = U1

instance (KnownNullability n, AsHaskellValue pg) => GDecodeSelectList (K1 R (PGDecoder (SqlT n pg))) where
  genericRowDecoder = K1 DecodePG

gselectListDecoder :: (Generic (hkd PGDecoder), GDecodeSelectList (Rep (hkd PGDecoder))) => hkd PGDecoder
gselectListDecoder = to genericRowDecoder
