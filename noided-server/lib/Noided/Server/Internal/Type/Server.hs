{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Server.Internal.Type.Server where

import Data.Foldable
import Network.Wai qualified as Wai
import Noided.Pathname
import Noided.Server.Internal.Type.Action
import Noided.Server.Internal.Type.Verb
import Noided.Server.Internal.Type.VerbRouter
import Optics.Core

data Server monad response
  = MkServer
  { allActions :: ![SomeAction monad response],
    notFoundAction :: Wai.Request -> monad response
  }

makeServer :: [SomeAction monad response] -> (Wai.Request -> monad response) -> Server monad response
makeServer = MkServer

mapServerResponse :: (Functor monad) => (response -> response') -> Server monad response -> Server monad response'
mapServerResponse f (MkServer aa nfa) =
  MkServer
    (fmap f <$> aa)
    (fmap f . nfa)

hoistServerMonad ::
  (forall a. m a -> n a) ->
  Server m response ->
  Server n response
hoistServerMonad f (MkServer aa nfa) =
  MkServer
    (hoistSomeActionMonad f <$> aa)
    (f . nfa)

newtype VerbRouterOf routed path = MkVerbRouterOf {getVerbRouterOf :: VerbRouter (routed path)}
  deriving (Semigroup, Monoid) via (VerbRouter (routed path))

type instance Index (VerbRouterOf routed path) = Verb

type instance IxValue (VerbRouterOf routed path) = (routed path)

instance Ixed (VerbRouterOf routed path)

instance At (VerbRouterOf routed path) where
  at verb = _VerbRouterOf % at verb

_VerbRouterOf :: Iso' (VerbRouterOf routed path) (VerbRouter (routed path))
_VerbRouterOf = coerced

_EmptyVerbRouterOf :: Prism' (VerbRouterOf routed path) ()
_EmptyVerbRouterOf = _VerbRouterOf % _EmptyVerbRouter

-- | Map some actions into a router, for much faster lookup.
someActionsToRouter :: forall monad response f. (Foldable f) => f (SomeAction monad response) -> Router (VerbRouterOf (Action monad response))
someActionsToRouter = foldl' f mempty
  where
    f :: Router (VerbRouterOf (Action monad response)) -> SomeAction monad response -> Router (VerbRouterOf (Action monad response))
    f router (SomeAction verb pt act) =
      set'
        (atRouter pt % non' (_VerbRouterOf % _EmptyVerbRouter) % at verb)
        (Just act)
        router
