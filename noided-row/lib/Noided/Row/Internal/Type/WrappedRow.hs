{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Noided.Row.Internal.Type.WrappedRow
  ( WrappedRow (UnsafeMkWrappedRow, WrappedRowCons, (:::%), (:::%?), EmptyWrappedRow),
    wrappedRowGetLabel,
    RowHasLabelWithType,
    wrappedRowGetFirstTyped,
    wrappedRowAppend,
    wrappedRowElementL,
    rowFoldMap1,
    WrappedElementLensOf (..),
    wrappedRowElementLenses,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as AK
import Data.Aeson.Types (Parser)
import Data.HKD
import Data.Kind (Constraint, Type)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Vector qualified as Vec
import GHC.Exts (Any)
import GHC.Records
import GHC.TypeLits
import Noided.Row.Internal.Type.RowElement
import Noided.Row.Internal.Type.RowLabel
import Optics.Core
import Text.Read
import Unsafe.Coerce (unsafeCoerce)

newtype WrappedRow (labels :: [RowLabel k]) (wrapper :: k -> Type)
  = UnsafeMkWrappedRow (Vec.Vector (wrapper Any))

wrappedHeadAndTail ::
  forall label wrapper val rest.
  WrappedRow (label :=> val ': rest) wrapper ->
  (wrapper val, WrappedRow rest wrapper)
wrappedHeadAndTail (UnsafeMkWrappedRow v) =
  case Vec.uncons v of
    Nothing -> error "impossible: vector was empty"
    Just (a, r) ->
      (unsafeCoerce a, unsafeCoerce r)

wrappedElementHeadAndTail ::
  forall label wrapper val rest.
  WrappedRow (label :=> val ': rest) wrapper ->
  (RowElement label (wrapper val), WrappedRow rest wrapper)
wrappedElementHeadAndTail (h :::% t) =
  (MkRowElement h, t)

infixr 5 `WrappedRowCons`

pattern WrappedRowCons ::
  forall name a wrapper rest.
  wrapper a ->
  WrappedRow rest wrapper ->
  WrappedRow (name :=> a ': rest) wrapper
pattern WrappedRowCons head' tail' <- (wrappedHeadAndTail -> (head', tail'))
  where
    WrappedRowCons head' (UnsafeMkWrappedRow r) =
      UnsafeMkWrappedRow (pure (unsafeCoerce head') <> r)

{-# COMPLETE WrappedRowCons #-}

infixr 5 :::%

pattern (:::%) ::
  forall name a wrapper rest.
  wrapper a ->
  WrappedRow rest wrapper ->
  WrappedRow (name :=> a ': rest) wrapper
pattern (:::%) h t = WrappedRowCons h t

{-# COMPLETE (:::%) #-}

infixr 5 :::%?

pattern (:::%?) ::
  forall name a wrapper rest.
  RowElement name (wrapper a) ->
  WrappedRow rest wrapper ->
  WrappedRow (name :=> a ': rest) wrapper
pattern (:::%?) v t <- (wrappedElementHeadAndTail -> (v, t))
  where
    (MkRowElement h) :::%? r = h :::% r
{-# INLINE (:::%?) #-}

{-# COMPLETE (:::%?) #-}

pattern EmptyWrappedRow ::
  forall wrapper.
  WrappedRow '[] wrapper
pattern EmptyWrappedRow <- (UnsafeMkWrappedRow _)
  where
    EmptyWrappedRow = UnsafeMkWrappedRow mempty

{-# COMPLETE EmptyWrappedRow #-}

type RowHasLabelWithType :: forall k. Symbol -> [RowLabel k] -> k -> Constraint
type RowHasLabelWithType label rowLabels t =
  ( KnownNat (RowIndexByLabelEvidenceIndex (RowLabelsIndexByLabel label rowLabels)),
    t ~ RowIndexByLabelEvidenceType (RowLabelsIndexByLabel label rowLabels)
  )

newtype WrappedElementLensOf labels t
  = WrappedElemLens {getWrappedElementLens :: forall wrapper. Lens' (WrappedRow labels wrapper) (wrapper t)}

wrappedRowElementLenses :: WrappedRow labels wrapper -> WrappedRow labels (WrappedElementLensOf labels)
wrappedRowElementLenses (UnsafeMkWrappedRow inner) = UnsafeMkWrappedRow (f <$> Vec.indexed inner)
  where
    f :: (Int, a) -> WrappedElementLensOf labels Any
    f (idx, _) =
      WrappedElemLens $
        lens
          (\(UnsafeMkWrappedRow storage) -> Vec.unsafeIndex storage idx)
          (\(UnsafeMkWrappedRow storage) ni -> UnsafeMkWrappedRow $ storage Vec.// [(idx, ni)])

rowFoldMap1 ::
  (Semigroup m) =>
  (forall a. wrapper a -> m) ->
  WrappedRow ((name :=> x) ': xs) wrapper ->
  m
rowFoldMap1 f (h :::% t) =
  case ffoldMap (Just . f) t of
    Just a -> f h <> a
    Nothing -> f h

wrappedRowElementL ::
  forall label rowLabels t wrapper.
  (RowHasLabelWithType label rowLabels t) =>
  Lens' (WrappedRow rowLabels wrapper) (wrapper t)
wrappedRowElementL = lens (wrappedRowGetLabel @label) (wrappedRowReplaceLabelMonomorphic @label)

instance
  ( RowHasLabelWithType recordLabel labels res,
    recordResult ~ wrapper res
  ) =>
  HasField recordLabel (WrappedRow labels wrapper) recordResult
  where
  getField = wrappedRowGetLabel @recordLabel

instance
  ( RowHasLabelWithType label labels origT,
    newLabels ~ RowLabelsReplaceLabelByType label newT labels,
    LabelOptic
      label
      A_Lens
      (WrappedRow newLabels wrapper)
      (WrappedRow labels wrapper)
      (wrapper newT)
      (wrapper origT)
  ) =>
  LabelOptic label A_Lens (WrappedRow labels wrapper) (WrappedRow newLabels wrapper) (wrapper origT) (wrapper newT)
  where
  labelOptic = lens (wrappedRowGetLabel @label) (wrappedRowReplaceLabel @label)
  {-# INLINEABLE labelOptic #-}

instance FFunctor (WrappedRow labels) where
  ffmap f (UnsafeMkWrappedRow vec) = UnsafeMkWrappedRow $ fmap f vec
  {-# INLINEABLE ffmap #-}

instance FFoldable (WrappedRow labels) where
  ffoldMap f (UnsafeMkWrappedRow vec) = foldMap f vec
  {-# INLINEABLE ffoldMap #-}
  flengthAcc acc (UnsafeMkWrappedRow vec) = acc + Vec.length vec

instance FTraversable (WrappedRow labels) where
  ftraverse f (UnsafeMkWrappedRow row) = UnsafeMkWrappedRow <$> traverse f row
  {-# INLINEABLE ftraverse #-}

instance FZip (WrappedRow labels) where
  fzipWith f (UnsafeMkWrappedRow l) (UnsafeMkWrappedRow r) =
    UnsafeMkWrappedRow $ Vec.zipWith f l r

instance (KnownNat (RowLabelsLength labels)) => FRepeat (WrappedRow labels) where
  frepeat r = UnsafeMkWrappedRow $ Vec.replicate len r
    where
      len = fromInteger $ natVal (Proxy @(RowLabelsLength labels))
  {-# INLINEABLE frepeat #-}

instance Eq (WrappedRow '[] wrapper) where
  _ == _ = True

instance Ord (WrappedRow '[] wrapper) where
  compare _ _ = EQ

instance Show (WrappedRow '[] wrapper) where
  showsPrec _ _ = showString "EmptyWrappedRow"

instance Read (WrappedRow '[] wrapper) where
  readListPrec = readListPrecDefault
  readPrec = do
    Ident "EmptyWrappedRow" <- lexP
    pure EmptyWrappedRow

instance
  (Eq (WrappedRow rest wrapper), Eq (wrapper a)) =>
  Eq (WrappedRow (label :=> a ': rest) wrapper)
  where
  (a :::% r) == (a' :::% r') =
    a == a' && r == r'

instance
  (Ord (WrappedRow rest wrapper), Ord (wrapper a)) =>
  Ord (WrappedRow (label :=> a ': rest) wrapper)
  where
  compare (a :::% r) (a' :::% r') =
    case compare a a' of
      LT -> LT
      GT -> GT
      EQ -> compare r r'

instance
  (Show (WrappedRow rest wrapper), Show (RowElement label (wrapper a))) =>
  Show (WrappedRow (label :=> a ': rest) wrapper)
  where
  showsPrec d (h :::%? r) =
    showParen (d > 5) $
      showsPrec 5 h
        . showString " :::%? "
        . showsPrec 5 r

instance
  ( Read (WrappedRow rest wrapper),
    Read (RowElement label (wrapper a)),
    KnownSymbol label,
    Read (wrapper a)
  ) =>
  Read (WrappedRow (label :=> a ': rest) wrapper)
  where
  readListPrec = readListPrecDefault
  readPrec = parens (prec 5 innerRead)
    where
      innerRead = do
        u <- step readPrec
        Symbol ":::%?" <- lexP
        v <- readPrec
        pure $ u :::%? v

instance Semigroup (WrappedRow '[] anything) where
  EmptyWrappedRow <> EmptyWrappedRow = EmptyWrappedRow

instance (Semigroup (WrappedRow rest w), Semigroup (w t)) => Semigroup (WrappedRow (l :=> t ': rest) w) where
  (a :::% r) <> (a' :::% r') =
    a <> a' :::% r <> r'

instance (Monoid (WrappedRow rest w), Monoid (w t)) => Monoid (WrappedRow (l :=> t ': rest) w) where
  mempty = mempty :::% mempty

instance Monoid (WrappedRow '[] anything) where
  mempty = EmptyWrappedRow

class WrappedRowToJSON labels wrapper where
  rowToJSON :: WrappedRow labels wrapper -> Map.Map Text.Text Int -> [(Key, Value)]
  rowToEncoding :: WrappedRow labels wrapper -> Map.Map Text.Text Int -> Series

instance WrappedRowToJSON '[] anything where
  rowToJSON _ _ = []
  rowToEncoding _ _ = mempty

instance
  ( ToJSON (wrapper a),
    KnownSymbol label,
    WrappedRowToJSON rest wrapper
  ) =>
  WrappedRowToJSON (label :=> a ': rest) wrapper
  where
  rowToJSON (a :::% r) m = (usedName .= a) : rowToJSON r newMap
    where
      newMap = m & at elementName % non 0 %~ (+ 1)
      usedName =
        AK.fromText $
          elementName
            <> maybe mempty (Text.pack . show) (Map.lookup elementName m)
      elementName = Text.pack (symbolVal $ Proxy @label)
  rowToEncoding (a :::% r) m = (usedName .= a) <> rowToEncoding r newMap
    where
      newMap = m & at elementName % non 0 %~ (+ 1)
      usedName =
        AK.fromText $
          elementName
            <> maybe mempty (Text.pack . show) (Map.lookup elementName m)
      elementName = Text.pack (symbolVal $ Proxy @label)

instance (WrappedRowToJSON labels wrapper) => ToJSON (WrappedRow labels wrapper) where
  toEncoding r = pairs $ rowToEncoding r mempty
  toJSON r = object $ rowToJSON r mempty

class WrappedRowFromJSON labels wrapper where
  rowFromJSON :: Object -> Map.Map Text.Text Int -> Parser (WrappedRow labels wrapper)

instance WrappedRowFromJSON '[] wrapper where
  rowFromJSON _ _ = pure EmptyWrappedRow

instance
  ( FromJSON (wrapper a),
    WrappedRowFromJSON rest wrapper,
    KnownSymbol label
  ) =>
  WrappedRowFromJSON (label :=> a ': rest) wrapper
  where
  rowFromJSON km m = (:::%) <$> km .: usedName <*> rowFromJSON km newMap
    where
      newMap = m & at elementName % non 0 %~ (+ 1)
      usedName =
        AK.fromText $
          elementName
            <> maybe mempty (Text.pack . show) (Map.lookup elementName m)
      elementName = Text.pack (symbolVal $ Proxy @label)

instance (WrappedRowFromJSON labels wrapper) => FromJSON (WrappedRow labels wrapper) where
  parseJSON = withObject "Row" $ \o -> rowFromJSON o mempty

wrappedRowGetLabel ::
  forall label t labels wrapper.
  (RowHasLabelWithType label labels t) =>
  WrappedRow labels wrapper ->
  wrapper t
wrappedRowGetLabel (UnsafeMkWrappedRow r) = unsafeCoerce $ Vec.unsafeIndex r idx
  where
    idx :: Int
    idx = fromInteger $ natVal (Proxy @(RowIndexByLabelEvidenceIndex (RowLabelsIndexByLabel label labels)))
{-# INLINE wrappedRowGetLabel #-}

wrappedRowReplaceLabelMonomorphic :: forall label labels t wrapper. (RowHasLabelWithType label labels t) => WrappedRow labels wrapper -> wrapper t -> WrappedRow labels wrapper
wrappedRowReplaceLabelMonomorphic (UnsafeMkWrappedRow r) wr =
  UnsafeMkWrappedRow $ r Vec.// [(idx, unsafeCoerce wr)]
  where
    idx :: Int
    idx = fromInteger $ natVal (Proxy @(RowIndexByLabelEvidenceIndex (RowLabelsIndexByLabel label labels)))

wrappedRowReplaceLabel ::
  forall {oldT} label newT labels newLabels wrapper.
  ( RowHasLabelWithType label labels oldT,
    newLabels ~ RowLabelsReplaceLabelByType label newT labels
  ) =>
  WrappedRow labels wrapper ->
  wrapper newT ->
  WrappedRow newLabels wrapper
wrappedRowReplaceLabel (UnsafeMkWrappedRow r) wr = UnsafeMkWrappedRow $ r Vec.// [(idx, unsafeCoerce wr)]
  where
    idx :: Int
    idx = fromInteger $ natVal (Proxy @(RowIndexByLabelEvidenceIndex (RowLabelsIndexByLabel label labels)))
{-# INLINE wrappedRowReplaceLabel #-}

wrappedRowGetFirstTyped ::
  forall {idx} t labels wrapper.
  ( idx ~ RowLabelsIndexByType t labels,
    KnownNat idx
  ) =>
  WrappedRow labels wrapper ->
  wrapper t
wrappedRowGetFirstTyped (UnsafeMkWrappedRow r) = unsafeCoerce $ Vec.unsafeIndex r idx
  where
    idx :: Int
    idx = fromInteger $ natVal (Proxy @idx)

wrappedRowAppend ::
  forall lhsLabels rhsLabels wrapper.
  WrappedRow lhsLabels wrapper ->
  WrappedRow rhsLabels wrapper ->
  WrappedRow (RowAppendLabels lhsLabels rhsLabels) wrapper
wrappedRowAppend (UnsafeMkWrappedRow l) (UnsafeMkWrappedRow r) =
  UnsafeMkWrappedRow (l <> r)
{-# INLINE wrappedRowAppend #-}
