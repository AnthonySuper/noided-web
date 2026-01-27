module Noided.Sql.Internal.Class.UnwrapSelectList where

import Data.HKD
import Data.Kind (Type)
import GHC.Generics
import Noided.Row
import Noided.Sql.Internal.Type.HaskellT
import Noided.Sql.Internal.Type.Tie

-- | Unwrap a select list type to a proper, non-HKD Haskell type.
class UnwrapSelectList selectList where
  type SelectListUnwrapped selectList :: Type
  unwrapSelectList :: selectList HaskellT -> SelectListUnwrapped selectList

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
