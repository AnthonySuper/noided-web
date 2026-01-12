{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Noided.Pathname.Internal.PathCaptures where

import Data.Kind (Type)
import Data.Type.Equality
import Type.Reflection
import Unsafe.Coerce

-- | Path captures are just a list of types.
type PathCaptures = [Type]

type family AppendPathCaptures (lhs :: [Type]) (rhs :: [Type]) where
  AppendPathCaptures '[] rhs = rhs
  AppendPathCaptures (x : xs) rhs = x ': AppendPathCaptures xs rhs

-- | Singleton type for path captures, used to write proofs.
type PathCaptureSing :: PathCaptures -> Type
data PathCaptureSing caps where
  PCSNil :: PathCaptureSing '[]
  (::$) :: !(TypeRep a) -> !(PathCaptureSing rest) -> PathCaptureSing (a ': rest)

instance TestEquality PathCaptureSing where
  testEquality PCSNil PCSNil = Just Refl
  testEquality (atr ::$ r) (atr' ::$ r') = do
    Refl <- testEquality atr atr'
    Refl <- testEquality r r'
    Just Refl
  testEquality _ _ = Nothing

class KnownPathCaptures (p :: PathCaptures) where
  pathCaptureSing :: PathCaptureSing p

instance KnownPathCaptures '[] where
  pathCaptureSing = PCSNil
  {-# NOINLINE [1] pathCaptureSing #-}

instance (Typeable a, KnownPathCaptures rest) => KnownPathCaptures (a ': rest) where
  pathCaptureSing = typeRep ::$ pathCaptureSing
  {-# NOINLINE [1] pathCaptureSing #-}

withKnownPathCaptures :: PathCaptureSing path -> ((KnownPathCaptures path) => a) -> a
withKnownPathCaptures PCSNil val = val
withKnownPathCaptures (tr ::$ r) val =
  withTypeable tr (withKnownPathCaptures r val)

proveAppendToSingletonIsCons :: AppendPathCaptures '[x] xs :~: x ': xs
proveAppendToSingletonIsCons = Refl

-- | Prove that appending nothing does nothing, using a singleton type.
-- We wind up using rewrite rules to eliminate these from the program so we don't
-- have to pay a cost at runtime.
proveAppendNothingDoesNothingSing :: PathCaptureSing a -> AppendPathCaptures a '[] :~: a
proveAppendNothingDoesNothingSing PCSNil = Refl
proveAppendNothingDoesNothingSing (_ ::$ r) =
  case proveAppendNothingDoesNothingSing r of
    Refl -> Refl
{-# NOINLINE [0] proveAppendNothingDoesNothingSing #-}

{-# RULES
"eraseProof/proveAppendNothingDoesNothing" [~1] forall p.
  proveAppendNothingDoesNothingSing p =
    p `seq` unsafeCoerce Refl
  #-}

proveAppendIsAssociativeSing ::
  forall a b c.
  PathCaptureSing a ->
  AppendPathCaptures (AppendPathCaptures a b) c :~: AppendPathCaptures a (AppendPathCaptures b c)
proveAppendIsAssociativeSing PCSNil = Refl
proveAppendIsAssociativeSing (_ ::$ r) =
  case proveAppendIsAssociativeSing @_ @b @c r of
    Refl -> Refl
{-# NOINLINE [0] proveAppendIsAssociativeSing #-}

{-# RULES
"eraseProof/proveAppendIsAssociativeSing" [~1] forall a.
  proveAppendIsAssociativeSing a =
    a `seq` unsafeCoerce Refl
  #-}
