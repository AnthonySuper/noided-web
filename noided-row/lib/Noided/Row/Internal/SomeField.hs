{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Row.Internal.SomeField
  ( RowFieldOf,
    RowHasFieldLabeled,
    rowFieldLabeled,
    rowFieldTypeRep,
    rowFieldLens,
    fromLabelPair,
    SomeField (..),
    withSomeField,
    withSomeFieldLens,
    hasFieldLabel,
    RowKnownFields,
    rowKnownFields,
    someFieldNamed,
    someFieldFieldName,
  )
where

import Control.Monad (guard)
import Data.Aeson
import Data.GADT.Compare
import Data.HKD
import Data.Monoid (First (..))
import Data.Proxy
import Data.Type.Equality
import Data.Vector qualified as Vec
import GHC.OverloadedLabels
import GHC.TypeLits
import Noided.Row.Internal.Type.RowLabel
import Noided.Row.Internal.Type.WrappedRow
import Optics.Core
import Type.Reflection
import Unsafe.Coerce

-- | A reference to a particular field of a row with the given labels and contained type.
data RowFieldOf labels contained where
  UnsafeMkRowFieldOf ::
    forall {k} (wrapped :: k) (labels :: [RowLabel k]).
    (Typeable wrapped) =>
    String ->
    Integer ->
    RowFieldOf labels wrapped

deriving instance Show (RowFieldOf labels contained)

instance Eq (RowFieldOf labels contained) where
  (UnsafeMkRowFieldOf _ i) == (UnsafeMkRowFieldOf _ i') =
    i == i'

instance Ord (RowFieldOf labels contained) where
  (UnsafeMkRowFieldOf _ p) `compare` (UnsafeMkRowFieldOf _ p') =
    p `compare` p'

instance ToJSON (RowFieldOf labels contained) where
  toEncoding (UnsafeMkRowFieldOf s p) = toEncoding (s, p)
  toJSON (UnsafeMkRowFieldOf s p) = toJSON (s, p)

instance TestEquality (RowFieldOf labels) where
  testEquality
    (UnsafeMkRowFieldOf @t _ _)
    (UnsafeMkRowFieldOf @t' _ _) = testEquality (typeRep @t) (typeRep @t')

instance GEq (RowFieldOf labels) where
  geq (UnsafeMkRowFieldOf @t lbl idx) (UnsafeMkRowFieldOf @t' lbl' idx') = do
    guard $ lbl == lbl'
    guard $ idx == idx'
    geq (typeRep @t) (typeRep @t')

instance GCompare (RowFieldOf labels) where
  gcompare (UnsafeMkRowFieldOf @t lbl idx) (UnsafeMkRowFieldOf @t' lbl' idx') =
    case compare lbl lbl' of
      LT -> GLT
      GT -> GGT
      EQ ->
        case compare idx idx' of
          LT -> GLT
          GT -> GGT
          EQ -> gcompare (typeRep @t) (typeRep @t')

instance (Typeable contained, RowKnownFields labels) => FromJSON (RowFieldOf labels contained) where
  parseJSON v = do
    (s, p) <- parseJSON v
    case getFirst (ffoldMap (First . matchLabelPair (typeRep @contained) s p) (rowKnownFields @labels)) of
      Just a -> return a
      Nothing -> fail "not a label"

type RowHasFieldLabeled labels label t n =
  ( RowLabelsIndexByLabel label labels ~ RowIndexOfLabel n t,
    RowIndexOrdinalE label t ~ RowLabelsIndexByOrdinal n labels,
    KnownSymbol label,
    KnownNat n,
    Typeable t
  )

instance
  (RowHasFieldLabeled labels label t n) =>
  IsLabel label (RowFieldOf labels t)
  where
  fromLabel = rowFieldLabeled (Proxy @label)

matchLabelPair :: TypeRep a -> String -> Integer -> RowFieldOf labels other -> Maybe (RowFieldOf labels a)
matchLabelPair tr st i r@(UnsafeMkRowFieldOf @t lbl idx) = do
  guard $ st == lbl
  guard $ i == idx
  Refl <- testEquality tr (typeRep @t)
  Just r

matchLabelPairSomeField :: String -> Integer -> RowFieldOf labels someType -> Maybe (SomeField labels)
matchLabelPairSomeField st i r@(UnsafeMkRowFieldOf lbl idx) = do
  guard $ st == lbl
  guard $ i == idx
  Just $ SomeField r

rowFieldLabeled ::
  forall {n} {t} label labels.
  (RowHasFieldLabeled labels label t n) =>
  Proxy label ->
  RowFieldOf labels t
rowFieldLabeled Proxy = UnsafeMkRowFieldOf (symbolVal (Proxy @label)) (natVal (Proxy @n))

rowFieldTypeRep :: RowFieldOf labels contained -> TypeRep contained
rowFieldTypeRep (UnsafeMkRowFieldOf @w _ _) = typeRep @w

rowFieldLens :: RowFieldOf labels contained -> Lens' (WrappedRow labels wrapper) (wrapper contained)
rowFieldLens (UnsafeMkRowFieldOf _ p) =
  let pv = fromInteger p
   in lens
        (\(UnsafeMkWrappedRow v) -> unsafeCoerce $ Vec.unsafeIndex v pv)
        (\(UnsafeMkWrappedRow items) ni -> UnsafeMkWrappedRow $ items Vec.// [(pv, unsafeCoerce ni)])

rowFieldLabel :: RowFieldOf labels contained -> String
rowFieldLabel (UnsafeMkRowFieldOf l _) = l

-- | A reference to a particular field of a row with an unknown type.
data SomeField labels where
  SomeField :: RowFieldOf labels contained -> SomeField labels

deriving instance Show (SomeField labels)

instance Eq (SomeField labels) where
  (SomeField (UnsafeMkRowFieldOf _ p)) == (SomeField (UnsafeMkRowFieldOf _ p')) =
    p == p'

instance Ord (SomeField labels) where
  (SomeField (UnsafeMkRowFieldOf _ p)) `compare` (SomeField (UnsafeMkRowFieldOf _ p')) =
    p `compare` p'

instance ToJSON (SomeField labels) where
  toEncoding r = withSomeField r toEncoding
  toJSON r = withSomeField r toJSON

instance (RowKnownFields labels) => FromJSON (SomeField labels) where
  parseJSON v = do
    (label, index) <- parseJSON v
    case getFirst (ffoldMap (First . matchLabelPairSomeField label index) (rowKnownFields @labels)) of
      Just a -> return a
      Nothing -> fail "not a valid field"

withSomeField :: SomeField labels -> (forall wrapped. RowFieldOf labels wrapped -> a) -> a
withSomeField (SomeField s) f = f s

withSomeFieldLens :: SomeField labels -> (forall wrapped. (forall wrapper. Lens' (WrappedRow labels wrapper) (wrapper wrapped)) -> a) -> a
withSomeFieldLens sf f' = withSomeField sf $ \rf -> f' (rowFieldLens rf)

someFieldFieldName :: SomeField labels -> String
someFieldFieldName (SomeField r) = rowFieldLabel r

hasFieldLabel :: String -> SomeField labels -> Bool
hasFieldLabel str (SomeField p) = str == rowFieldLabel p

class RowKnownFieldsRec knownLabels labels where
  unsafeRowKnownFieldsRec ::
    Integer ->
    WrappedRow labels (RowFieldOf knownLabels)

instance RowKnownFieldsRec labels '[] where
  unsafeRowKnownFieldsRec _ = EmptyWrappedRow

instance
  (KnownSymbol sym, Typeable t, RowKnownFieldsRec labels rest) =>
  RowKnownFieldsRec labels (sym :=> t ': rest)
  where
  unsafeRowKnownFieldsRec i = unsafeSingle :::% unsafeRowKnownFieldsRec (i + 1)
    where
      unsafeSingle = UnsafeMkRowFieldOf (symbolVal $ Proxy @sym) i

type RowKnownFields labels = RowKnownFieldsRec labels labels

rowKnownFields :: (RowKnownFields labels) => WrappedRow labels (RowFieldOf labels)
rowKnownFields = unsafeRowKnownFieldsRec 0

fromLabelPair :: (RowKnownFields labels, Typeable contained) => String -> Integer -> Maybe (RowFieldOf labels contained)
fromLabelPair st i = getFirst (ffoldMap (First . matchLabelPair typeRep st i) rowKnownFields)

someFieldNamed :: forall labels. (RowKnownFields labels) => String -> Maybe (SomeField labels)
someFieldNamed n = getFirst $ ffoldMap f (rowKnownFields @labels)
  where
    f :: forall wrapped. RowFieldOf labels wrapped -> First (SomeField labels)
    f rf
      | rowFieldLabel rf == n = First $ Just $ SomeField rf
      | otherwise = mempty
