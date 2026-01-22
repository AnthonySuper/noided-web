{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Row.Internal.HKDWrapping where

import Data.Coerce
import Data.Functor.Identity
import Data.HKD
import Data.Kind (Type)
import GHC.Generics
import Noided.Row.Internal.Type.Row
import Noided.Row.Internal.Type.WrappedRow
import Optics.Iso

infixr 6 :**:

-- | Non-functor equivalent of '(:*:)'.
data (:**:) a b = a :**: b
  deriving (Show, Read, Eq, Ord, Generic, Functor)

class GHKDWrap tr hkdr where
  genericHkdWrap :: tr a -> hkdr a

instance (GHKDWrap i hkdi) => GHKDWrap (M1 c md i) (M1 c md hkdi) where
  genericHkdWrap (M1 a) = M1 $ genericHkdWrap a

instance
  (GHKDWrap lhs hkdlhs, GHKDWrap rhs hkdrhs) =>
  GHKDWrap (lhs :*: rhs) (hkdlhs :*: hkdrhs)
  where
  genericHkdWrap (l :*: r) = genericHkdWrap l :*: genericHkdWrap r

instance
  (GHKDWrap lhs hkdlhs, GHKDWrap rhs hkdrhs) =>
  GHKDWrap (lhs :+: rhs) (hkdlhs :+: hkdrhs)
  where
  genericHkdWrap = \case
    L1 l -> L1 $ genericHkdWrap l
    R1 r -> R1 $ genericHkdWrap r

instance GHKDWrap V1 V1 where
  genericHkdWrap x = case x of {}

instance GHKDWrap U1 U1 where
  genericHkdWrap U1 = U1

instance GHKDWrap (Rec0 a) (Rec0 (Identity a)) where
  genericHkdWrap (K1 a) = K1 (Identity a)

instance (HKDWrap r, k ~ HKDWrapped r) => GHKDWrap (Rec0 r) (Rec0 (k Identity)) where
  genericHkdWrap (K1 a) = K1 (hkdWrap a)

gHkdWrap ::
  ( Generic t,
    Generic (hkd Identity),
    GHKDWrap (Rep t) (Rep (hkd Identity))
  ) =>
  t ->
  hkd Identity
gHkdWrap = to @_ @() . genericHkdWrap . from

-- | Class for types that have an equivalent HKD type.
class HKDWrap (t :: Type) where
  -- | The HKD that matches with this type.
  type HKDWrapped t :: ((Type -> Type) -> Type)

  -- | Transform this type into its equivalent HKD type, indexed by the 'Identity' functor.
  hkdWrap :: t -> (HKDWrapped t) Identity
  default hkdWrap ::
    (Generic t, Generic (HKDWrapped t Identity), GHKDWrap (Rep t) (Rep (HKDWrapped t Identity))) =>
    t ->
    HKDWrapped t Identity
  hkdWrap = gHkdWrap

class GHKDUnwrap hkd t where
  genericHkdUnwrap :: hkd () -> t ()

instance (GHKDUnwrap hinner tinner) => GHKDUnwrap (M1 c md hinner) (M1 c md tinner) where
  genericHkdUnwrap (M1 i) = M1 $ genericHkdUnwrap i

instance
  (GHKDUnwrap lhs hkdlhs, GHKDUnwrap rhs hkdrhs) =>
  GHKDUnwrap (lhs :*: rhs) (hkdlhs :*: hkdrhs)
  where
  genericHkdUnwrap (l :*: r) = genericHkdUnwrap l :*: genericHkdUnwrap r

instance
  (GHKDUnwrap lhs hkdlhs, GHKDUnwrap rhs hkdrhs) =>
  GHKDUnwrap (lhs :+: rhs) (hkdlhs :+: hkdrhs)
  where
  genericHkdUnwrap = \case
    L1 l -> L1 $ genericHkdUnwrap l
    R1 r -> R1 $ genericHkdUnwrap r

instance GHKDUnwrap V1 V1 where
  genericHkdUnwrap x = case x of {}

instance GHKDUnwrap U1 U1 where
  genericHkdUnwrap U1 = U1

instance (r ~ r') => GHKDUnwrap (Rec0 (Identity r')) (Rec0 r) where
  genericHkdUnwrap = coerce

instance
  (HKDUnwrap hkd, w ~ Identity, res ~ HKDUnwrapped hkd) =>
  GHKDUnwrap (Rec0 (hkd w)) (Rec0 res)
  where
  genericHkdUnwrap (K1 v) = K1 $ hkdUnwrap v

gHkdUnwrap ::
  ( Generic (hkd Identity),
    Generic a,
    GHKDUnwrap (Rep (hkd Identity)) (Rep a)
  ) =>
  hkd Identity ->
  a
gHkdUnwrap = to @_ @() . genericHkdUnwrap . from

-- | A class for HKDs that can be unwrapped into non-HKDs.
class HKDUnwrap (hkd :: ((Type -> Type) -> Type)) where
  -- | The non-HKD type.
  type HKDUnwrapped hkd :: Type

  -- | Unwrap the HKD into a normal type.
  hkdUnwrap :: hkd Identity -> HKDUnwrapped hkd
  default hkdUnwrap ::
    ( Generic (HKDUnwrapped hkd),
      Generic (hkd Identity),
      GHKDUnwrap (Rep (hkd Identity)) (Rep (HKDUnwrapped hkd))
    ) =>
    hkd Identity ->
    HKDUnwrapped hkd
  hkdUnwrap = gHkdUnwrap

-- | Unwrap an HKD into a sequence.
hkdSequenceUnwrap :: (HKDUnwrap hkd, FTraversable hkd, Applicative f) => hkd f -> f (HKDUnwrapped hkd)
hkdSequenceUnwrap = fmap hkdUnwrap . ftraverse (Identity <$>)

instance HKDWrap (Row labels) where
  type HKDWrapped (Row labels) = WrappedRow labels
  hkdWrap (MkRow r) = r

instance HKDUnwrap (WrappedRow labels) where
  type HKDUnwrapped (WrappedRow labels) = Row labels
  hkdUnwrap = MkRow

instance HKDWrap (Identity a) where
  type HKDWrapped (Identity a) = Element a
  hkdWrap = Element

instance HKDUnwrap (Element a) where
  type HKDUnwrapped (Element a) = Identity a
  hkdUnwrap = coerce

instance (HKDWrap l, HKDWrap r) => HKDWrap (l :**: r) where
  type HKDWrapped (l :**: r) = HKDWrapped l :*: HKDWrapped r
  hkdWrap (l :**: r) = hkdWrap l :*: hkdWrap r

instance (HKDUnwrap l, HKDUnwrap r) => HKDUnwrap (l :*: r) where
  type HKDUnwrapped (l :*: r) = HKDUnwrapped l :**: HKDUnwrapped r
  hkdUnwrap (l :*: r) = hkdUnwrap l :**: hkdUnwrap r

gHkdWrapped ::
  ( GHKDWrap (Rep s) (Rep (hkd1 Identity)),
    Generic s,
    Generic t,
    Generic (hkd1 Identity),
    Generic (hkd2 Identity),
    GHKDUnwrap (Rep (hkd2 Identity)) (Rep t)
  ) =>
  Iso s t (hkd1 Identity) (hkd2 Identity)
gHkdWrapped = iso gHkdWrap gHkdUnwrap

gHkdUnwrapped ::
  ( GHKDUnwrap (Rep (hkd1 Identity)) (Rep a),
    Generic a,
    Generic b,
    Generic (hkd1 Identity),
    Generic (hkd2 Identity),
    GHKDWrap (Rep b) (Rep (hkd2 Identity))
  ) =>
  Iso (hkd1 Identity) (hkd2 Identity) a b
gHkdUnwrapped = iso gHkdUnwrap gHkdWrap

hkdWrapped ::
  ( HKDWrap s,
    HKDUnwrap hkd
  ) =>
  Iso s (HKDUnwrapped hkd) (HKDWrapped s Identity) (hkd Identity)
hkdWrapped = iso hkdWrap hkdUnwrap

hkdUnwrapped ::
  ( HKDUnwrap hkd,
    HKDWrap b
  ) =>
  Iso (hkd Identity) (HKDWrapped b Identity) (HKDUnwrapped hkd) b
hkdUnwrapped = iso hkdUnwrap hkdWrap
