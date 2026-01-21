{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Noided.Row.Internal.Type.Row
  ( Row (MkRow, EmptyRow, (:::), (:::?)),
    rowAppend,
    rowApplyWrapper,
  )
where

import Data.Aeson
import Data.Coerce
import Data.Functor.Identity
import GHC.Records
import GHC.TypeLits (KnownSymbol)
import Noided.Row.Internal.Type.RowElement (RowElement (..))
import Noided.Row.Internal.Type.RowLabel
import Noided.Row.Internal.Type.WrappedRow
import Optics.Core
import Text.Read
import Unsafe.Coerce

newtype Row labels = MkRow (WrappedRow labels Identity)

toWrapped :: Row labels -> WrappedRow labels Identity
toWrapped = coerce

rowWrapped :: Iso (Row labels) (Row newLabels) (WrappedRow labels Identity) (WrappedRow newLabels Identity)
rowWrapped = coerced

pattern EmptyRow :: Row '[]
pattern EmptyRow = MkRow EmptyWrappedRow

{-# COMPLETE EmptyRow #-}

unconsRow ::
  forall name a rest.
  Row (name :=> a ': rest) ->
  (a, Row rest)
unconsRow (MkRow (Identity a :::% r)) = (a, MkRow r)

unconsRowElement ::
  forall name a rest.
  Row (name :=> a ': rest) ->
  (RowElement name a, Row rest)
unconsRowElement (MkRow (a :::%? r)) =
  (fmap runIdentity a, MkRow r)

infixr 5 :::

pattern (:::) ::
  forall name a rest.
  a ->
  Row rest ->
  Row (name :=> a ': rest)
pattern (:::) a r <- (unconsRow -> (a, r))
  where
    (:::) a (MkRow r) = MkRow (Identity a :::% r)

{-# COMPLETE (:::) #-}

infixr 5 :::?

pattern (:::?) ::
  forall name a rest.
  RowElement name a ->
  Row rest ->
  Row (name :=> a ': rest)
pattern (:::?) a r <- (unconsRowElement -> (a, r))
  where
    (:::?) a (MkRow r) = MkRow (fmap Identity a :::%? r)

{-# COMPLETE (:::?) #-}

deriving instance (Eq (WrappedRow labels Identity)) => Eq (Row labels)

deriving instance (Ord (WrappedRow labels Identity)) => Ord (Row labels)

instance Show (Row '[]) where
  showsPrec _ _ = showString "EmptyRow"

instance Read (Row '[]) where
  readListPrec = readListPrecDefault
  readPrec = do
    Ident "EmptyRow" <- lexP
    pure EmptyRow

instance
  (Show (Row rest), Show (RowElement label a)) =>
  Show (Row (label :=> a ': rest))
  where
  showsPrec d (h :::? r) =
    showParen (d > 5) $
      showsPrec 5 h
        . showString " :::? "
        . showsPrec 5 r

instance
  ( Read (Row rest),
    Read (RowElement label a),
    KnownSymbol label,
    Read a
  ) =>
  Read (Row (label :=> a ': rest))
  where
  readListPrec = readListPrecDefault
  readPrec = parens $ prec 5 $ do
    u <- step readPrec
    Symbol ":::?" <- lexP
    (u :::?) <$> readPrec

instance
  (HasField recordLabel (WrappedRow labels Identity) (Identity a)) =>
  HasField recordLabel (Row labels) a
  where
  getField = runIdentity . getField @recordLabel . toWrapped

instance
  (LabelOptic label A_Lens (WrappedRow labels Identity) (WrappedRow newLabels Identity) (Identity a) (Identity b)) =>
  LabelOptic label A_Lens (Row labels) (Row newLabels) a b
  where
  labelOptic = rowWrapped % labelOptic @label % coerced
  {-# INLINE labelOptic #-}

instance (ToJSON (WrappedRow labels Identity)) => ToJSON (Row labels) where
  toEncoding = toEncoding . toWrapped
  toJSON = toJSON . toWrapped

instance (FromJSON (WrappedRow labels Identity)) => FromJSON (Row labels) where
  parseJSON = fmap MkRow . parseJSON

rowAppend ::
  forall lhsLabels rhsLabels.
  Row lhsLabels ->
  Row rhsLabels ->
  Row (RowAppendLabels lhsLabels rhsLabels)
rowAppend = coerce (wrappedRowAppend @lhsLabels @rhsLabels @Identity)

rowApplyWrapper ::
  forall wrappedLabels wrapper.
  WrappedRow wrappedLabels wrapper ->
  Row (RowLabelsApplyWrapper wrapper wrappedLabels)
rowApplyWrapper (UnsafeMkWrappedRow elems) =
  MkRow (UnsafeMkWrappedRow (fmap unsafeCoerce elems))
