{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Sql.Internal.Class.UnwrapSelectList where

import Data.HKD
import Data.Kind (Type)
import GHC.Generics
import GHC.Records
import Noided.Row
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Type.HaskellT
import Noided.Sql.Internal.Type.Tie

-- | Unwrap a select list type to a proper, non-HKD Haskell type.
-- This is because working with HKDs in Haskell is, generally, extremely irritating.
-- So it\'s nice to unwrap to some normal type that's easier to work with.
class UnwrapSelectList selectList where
  type SelectListUnwrapped selectList :: Type
  unwrapSelectList :: selectList HaskellT -> SelectListUnwrapped selectList
  default unwrapSelectList ::
    ( Generic (SelectListUnwrapped selectList),
      GUnwrapSelectList selectList (Rep (SelectListUnwrapped selectList))
    ) =>
    selectList HaskellT ->
    SelectListUnwrapped selectList
  unwrapSelectList = gunwrapSelectList

instance UnwrapSelectList (Element sqlT) where
  type SelectListUnwrapped (Element sqlT) = HaskellValueType sqlT
  unwrapSelectList (Element (HaskT e)) = e

instance (UnwrapSelectList lhs, UnwrapSelectList rhs) => UnwrapSelectList (lhs :*: rhs) where
  type SelectListUnwrapped (lhs :*: rhs) = SelectListUnwrapped lhs :**: SelectListUnwrapped rhs
  unwrapSelectList (lhs :*: rhs) =
    unwrapSelectList lhs :**: unwrapSelectList rhs

instance (UnwrapSelectList lhs, UnwrapSelectList rhs) => UnwrapSelectList (lhs :-: rhs) where
  type SelectListUnwrapped (lhs :-: rhs) = SelectListUnwrapped lhs :--: SelectListUnwrapped rhs
  unwrapSelectList (lhs :-: rhs) =
    unwrapSelectList lhs :--: unwrapSelectList rhs

class GUnwrapSelectList selectList res where
  genericUnwrapSelectList :: selectList HaskellT -> res ()

instance GUnwrapSelectList selectList U1 where
  genericUnwrapSelectList _ = U1

instance
  (GUnwrapSelectList selectList lhs, GUnwrapSelectList selectList rhs) =>
  GUnwrapSelectList selectList (lhs :*: rhs)
  where
  genericUnwrapSelectList sl =
    genericUnwrapSelectList sl :*: genericUnwrapSelectList sl

instance
  (GUnwrapSelectList selectList inner) =>
  GUnwrapSelectList selectList (M1 D md inner)
  where
  genericUnwrapSelectList = M1 . genericUnwrapSelectList

instance
  (GUnwrapSelectList selectList inner) =>
  GUnwrapSelectList selectList (M1 C md inner)
  where
  genericUnwrapSelectList = M1 . genericUnwrapSelectList

instance
  ( HasField fieldName (selectList HaskellT) (HaskellT columnType),
    HaskellValueType columnType ~ resultType
  ) =>
  GUnwrapSelectList selectList (M1 s (MetaSel (Just fieldName) ignored ignored'' ignored''') (Rec0 resultType))
  where
  genericUnwrapSelectList =
    M1 . K1 . getHaskT . getField @fieldName

gunwrapSelectList ::
  forall result selectList.
  ( Generic result,
    GUnwrapSelectList selectList (Rep result)
  ) =>
  selectList HaskellT ->
  result
gunwrapSelectList = to . genericUnwrapSelectList
