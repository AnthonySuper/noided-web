{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Row.Internal.HKDToRow where

import Data.HKD
import Data.Kind (Type)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Noided.Row.Internal.Type.RowLabel
import Noided.Row.Internal.Type.WrappedRow

data VoidWrapper (a :: k)

-- | Helper type: differentiates between wrapped fields of an HKD, and sub-HKD-fields of an HKD
data Rec0ValueType valType where
  Rec0HKD :: Rec0ValueType (k -> Type)
  Rec0Val :: Rec0ValueType k

data Rec0Unpack = forall k. Rec0Up (Rec0ValueType k) (k -> Type) k

type HKDOf k = (k -> Type) -> Type

type WrapperOf k = k -> Type

-- | Matches on the inner type in a 'Rec0' to determine if that field is a wrapped value,
-- or a sub-HKD.
type family Rec0ValDecide w where
  Rec0ValDecide ((wrapper :: (HKDOf k)) (val :: (k -> Type))) = Rec0Up Rec0HKD wrapper val
  Rec0ValDecide ((wrapper :: (WrapperOf k)) (val :: k)) = Rec0Up Rec0Val wrapper val
  Rec0ValDecide w =
    TypeError (Text "Unable to use " :<>: ShowType w :<>: Text " as an HKD row value")

type family Rec0UnpackToRL s k where
  Rec0UnpackToRL _ (Rec0Up Rec0HKD hkd wrap) = HKDRowLabels hkd
  Rec0UnpackToRL (Just sym) (Rec0Up Rec0Val wrap f) = '[sym :=> f]

type family Rec0UnpackArg k where
  Rec0UnpackArg (Rec0Up Rec0Val w f) = w f
  Rec0UnpackArg (Rec0Up Rec0HKD w f) = w f

class Rec0ValUnpack s k w | s k -> w where
  rec0UnpackToRL :: Proxy s -> Proxy k -> Rec0UnpackArg k -> WrappedRow (Rec0UnpackToRL s k) w

instance (wrap ~ wrap', HKDToRow hkd) => Rec0ValUnpack a (Rec0Up Rec0HKD hkd wrap) wrap' where
  rec0UnpackToRL _ _ = hkdToRow

instance (wrap ~ wrap') => Rec0ValUnpack (Just a) (Rec0Up Rec0Val wrap f) wrap' where
  rec0UnpackToRL _ _ a = a :::% EmptyWrappedRow

type GHKDRowLabels ::
  forall genericK k.
  (genericK -> Type) ->
  (k -> Type) ->
  [RowLabel k]
type family GHKDRowLabels r w where
  GHKDRowLabels (D1 _ i) w = GHKDRowLabels i w
  GHKDRowLabels (S1 (MetaSel s _ _ _) (Rec0 wv)) wrapper =
    Rec0UnpackToRL s (Rec0ValDecide wv)
  GHKDRowLabels (C1 _ i) w = GHKDRowLabels i w
  GHKDRowLabels (a :*: b) w = RowAppendLabels (GHKDRowLabels a w) (GHKDRowLabels b w)

class GHKDToRow rep wrapper | rep -> wrapper where
  ghkdToRow :: rep a -> WrappedRow (GHKDRowLabels rep wrapper) wrapper

instance (GHKDToRow inner wrapper) => GHKDToRow (D1 md inner) wrapper where
  ghkdToRow (M1 c) = ghkdToRow c

instance (GHKDToRow inner wrapper) => GHKDToRow (C1 mc inner) wrapper where
  ghkdToRow (M1 c) = ghkdToRow c

instance
  (GHKDToRow lhs wrapper, GHKDToRow rhs wrapper) =>
  GHKDToRow (lhs :*: rhs) wrapper
  where
  ghkdToRow (lhs :*: rhs) = wrappedRowAppend (ghkdToRow lhs) (ghkdToRow rhs)

instance
  ( Rec0ValUnpack sel (Rec0ValDecide v) wrapper',
    Rec0UnpackArg (Rec0ValDecide v) ~ v
  ) =>
  GHKDToRow (S1 (MetaSel sel ignore ignore' ignored') (Rec0 v)) wrapper'
  where
  ghkdToRow (M1 (K1 a)) =
    rec0UnpackToRL
      (Proxy @sel)
      (Proxy @(Rec0ValDecide v))
      a

class (GHKDToRow (Rep (hkd wrapper)) wrapper, Generic (hkd wrapper)) => RepGHKDToRow hkd wrapper

instance (GHKDToRow (Rep (hkd wrapper)) wrapper, Generic (hkd wrapper)) => RepGHKDToRow hkd wrapper

class HKDToRow (hkd :: (k -> Type) -> Type) where
  type HKDRowLabels hkd :: [RowLabel k]
  type HKDRowLabels hkd = GHKDRowLabels (Rep (hkd VoidWrapper)) VoidWrapper
  hkdToRow :: hkd wrapper -> WrappedRow (HKDRowLabels hkd) wrapper
  default hkdToRow ::
    forall wrapper.
    ( HKDRowLabels hkd ~ GHKDRowLabels (Rep (hkd wrapper)) wrapper,
      RepGHKDToRow hkd wrapper
    ) =>
    hkd wrapper ->
    WrappedRow (HKDRowLabels hkd) wrapper
  hkdToRow = ghkdToRow . from

instance HKDToRow (Element i) where
  type HKDRowLabels (Element i) = '["element" :=> i]
  hkdToRow (Element e) = e :::% EmptyWrappedRow

instance HKDToRow (WrappedRow labels) where
  type HKDRowLabels (WrappedRow labels) = labels
  hkdToRow = id
