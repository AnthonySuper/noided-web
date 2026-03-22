{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Noided.Row.Internal.Type.RowElement where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.OverloadedLabels
import GHC.Read (expectP)
import GHC.TypeLits
import Text.Read

-- | One single element of a row.
newtype RowElement (label :: Symbol) (value :: Type) where
  MkRowElement :: forall label value. value -> RowElement label value
  deriving (Functor)

instance (Show value, KnownSymbol label) => Show (RowElement label value) where
  showsPrec d (MkRowElement r) =
    showParen (d > 6) $
      showString "#"
        . showString (symbolVal (Proxy @label))
        . showString " :==> "
        . showsPrec 6 r

instance (Read value, KnownSymbol label) => Read (RowElement label value) where
  readListPrec = readListPrecDefault
  readPrec = parens $ prec 6 $ do
    Symbol "#" <- lexP
    expectP $ Ident (symbolVal $ Proxy @label)
    expectP $ Symbol ":==>"
    MkRowElement <$> step readPrec

rowElement :: forall (label :: Symbol) (value :: Type). value -> RowElement label value
rowElement = MkRowElement

data RowElementLabel (s :: Symbol)

rowElementLabel :: forall s. RowElementLabel s
rowElementLabel = rowElementLabel

decomposeRowElement :: RowElement label value -> (RowElementLabel label, value)
decomposeRowElement (MkRowElement v) = (rowElementLabel, v)

infix 6 :==>

pattern (:==>) :: RowElementLabel s -> value -> RowElement s value
pattern (:==>) proxy v <- (decomposeRowElement -> (proxy, v))
  where
    (:==>) _ v = MkRowElement v

{-# COMPLETE (:==>) #-}

instance (l ~ l') => IsLabel l (RowElementLabel l') where
  fromLabel = rowElementLabel
