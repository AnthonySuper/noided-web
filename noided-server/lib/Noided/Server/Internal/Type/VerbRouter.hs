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
import Network.HTTP.Types
import Optics.Core
import Prelude hiding (lookup)

-- | Router type that maps a verb onto some specific key.
-- This is actually implemented as a datatype and not a map to get constant-time
-- lookup and constant space.
data VerbRouter routed
  = RouteVerbs
  { routeGet :: Maybe routed,
    routePost :: Maybe routed,
    routeHead :: Maybe routed,
    routePut :: Maybe routed,
    routeDelete :: Maybe routed,
    routeTrace :: Maybe routed,
    routeConnect :: Maybe routed,
    routeOptions :: Maybe routed,
    routePatch :: Maybe routed
  }
  deriving (Functor, Foldable, Traversable)

_EmptyVerbRouter :: Prism' (VerbRouter routed) ()
_EmptyVerbRouter = prism' mempty $ \case
  (RouteVerbs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) -> Just ()
  _ -> Nothing

-- | Semigroup instance: first present key wins
instance Semigroup (VerbRouter routed) where
  (RouteVerbs g p h pu d t c o pa) <> (RouteVerbs g' p' h' pu' d' t' c' o' pa') =
    RouteVerbs
      (g <|> g')
      (p <|> p')
      (h <|> h')
      (pu <|> pu')
      (d <|> d')
      (t <|> t')
      (c <|> c')
      (o <|> o')
      (pa <|> pa')

instance Monoid (VerbRouter routed) where
  mempty = RouteVerbs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

type instance Index (VerbRouter routed) = StdMethod

type instance IxValue (VerbRouter routed) = routed

instance Ixed (VerbRouter routed)

instance At (VerbRouter routed) where
  at verb = lensVL $ \f s@(RouteVerbs g p h pu d t c o pa) ->
    case verb of
      GET -> f g <&> \v -> s {routeGet = v}
      POST -> f p <&> \v -> s {routePost = v}
      HEAD -> f h <&> \v -> s {routeHead = v}
      PUT -> f pu <&> \v -> s {routePut = v}
      DELETE -> f d <&> \v -> s {routeDelete = v}
      TRACE -> f t <&> \v -> s {routeTrace = v}
      CONNECT -> f c <&> \v -> s {routeConnect = v}
      OPTIONS -> f o <&> \v -> s {routeOptions = v}
      PATCH -> f pa <&> \v -> s {routePatch = v}

lookup :: StdMethod -> VerbRouter routed -> Maybe routed
lookup v = view (at v)

singleton :: StdMethod -> routed -> VerbRouter routed
singleton v r = mempty & at v ?~ r
