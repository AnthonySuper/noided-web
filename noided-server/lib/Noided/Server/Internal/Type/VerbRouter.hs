{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Server.Internal.Type.VerbRouter
  ( VerbRouter,
    _EmptyVerbRouter,
    lookup,
    singleton,
  )
where

import Control.Applicative ((<|>))
import Noided.Server.Internal.Type.Verb
import Optics.Core
import Prelude hiding (lookup)

-- | Router type that maps a verb onto some specific key.
-- This is actually implemented as a datatype and not a map to get constant-time
-- lookup and constant space.
data VerbRouter routed
  = RouteVerbs
  { routeGet :: Maybe routed,
    routePost :: Maybe routed,
    routePut :: Maybe routed,
    routePatch :: Maybe routed,
    routeDelete :: Maybe routed,
    routeOptions :: Maybe routed
  }
  deriving (Functor, Foldable, Traversable)

_EmptyVerbRouter :: Prism' (VerbRouter routed) ()
_EmptyVerbRouter = prism' mempty $ \case
  (RouteVerbs Nothing Nothing Nothing Nothing Nothing Nothing) -> Just ()
  _ -> Nothing

-- | Semigroup instance: first present key wins
instance Semigroup (VerbRouter routed) where
  (RouteVerbs g p pu pa d o) <> (RouteVerbs g' p' pu' pa' d' o') =
    RouteVerbs
      (g <|> g')
      (p <|> p')
      (pu <|> pu')
      (pa <|> pa')
      (d <|> d')
      (o <|> o')

instance Monoid (VerbRouter routed) where
  mempty = RouteVerbs Nothing Nothing Nothing Nothing Nothing Nothing

type instance Index (VerbRouter routed) = Verb

type instance IxValue (VerbRouter routed) = routed

instance Ixed (VerbRouter routed)

instance At (VerbRouter routed) where
  at verb = lensVL $ \f s@(RouteVerbs g p pu pa d o) ->
    case verb of
      GET -> f g <&> \v -> s {routeGet = v}
      POST -> f p <&> \v -> s {routePost = v}
      PUT -> f pu <&> \v -> s {routePut = v}
      PATCH -> f pa <&> \v -> s {routePatch = v}
      DELETE -> f d <&> \v -> s {routeDelete = v}
      OPTIONS -> f o <&> \v -> s {routeOptions = v}

lookup :: Verb -> VerbRouter routed -> Maybe routed
lookup v = view (at v)

singleton :: Verb -> routed -> VerbRouter routed
singleton v r = mempty & at v ?~ r
