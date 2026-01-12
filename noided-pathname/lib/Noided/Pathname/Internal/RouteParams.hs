{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Noided.Pathname.Internal.RouteParams where

import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Noided.Pathname.Internal.PathCaptures

-- | Params for a matched route.
-- This is equivalent to the common @ HList @ type, but specified here in case we want to add more to it later.
data RouteParams (t :: [Type]) where
  -- | The end of a route params list
  RPNil :: RouteParams '[]
  -- | Cons for a route params list
  (:-$) :: t -> RouteParams rest -> RouteParams (t ': rest)

infixl 3 :-$

deriving instance Show (RouteParams '[])

deriving instance (Show t, Show (RouteParams rest)) => Show (RouteParams (t ': rest))

deriving instance Eq (RouteParams '[])

deriving instance (Eq t, Eq (RouteParams rest)) => Eq (RouteParams (t ': rest))

deriving instance Ord (RouteParams '[])

deriving instance (Ord t, Ord (RouteParams rest)) => Ord (RouteParams (t ': rest))

instance NFData (RouteParams '[]) where
  rnf RPNil = ()

instance (NFData t, NFData (RouteParams rest)) => NFData (RouteParams (t ': rest)) where
  rnf (t :-$ rest) = rnf t `seq` rnf rest

-- | Append two sets of route params.
-- This is equivalent to '++' for lists, but type-indexed.
appendRouteParams :: RouteParams lhs -> RouteParams rhs -> RouteParams (AppendPathCaptures lhs rhs)
appendRouteParams RPNil a = a
appendRouteParams (a :-$ t) r =
  a :-$ appendRouteParams t r
