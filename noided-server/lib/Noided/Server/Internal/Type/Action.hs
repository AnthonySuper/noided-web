{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Server.Internal.Type.Action where

import Network.HTTP.Types (StdMethod)
import Noided.Pathname
import Noided.Server.Internal.Type.Request

-- | An action for some URL with path params.
newtype Action monad response pathParams = Act {runAct :: Request pathParams -> monad response}

mapActionResponse :: (Functor monad) => (a -> response) -> Action monad a pathParams -> Action monad response pathParams
mapActionResponse f (Act action) = Act $ fmap f . action

transformAction ::
  ( (Request pathParams1 -> monad1 response1) -> Request pathParams2 -> monad2 response2
  ) ->
  Action monad1 response1 pathParams1 ->
  Action monad2 response2 pathParams2
transformAction h (Act act) = Act $ h act

hoistActionMonad ::
  (forall a. m a -> n a) ->
  Action m response pathParams ->
  Action n response pathParams
hoistActionMonad h (Act act) = Act (h . act)

-- | Some action, at a verb and a path.
data SomeAction monad response where
  SomeAction ::
    StdMethod ->
    PathTemplate pathParams ->
    Action monad response pathParams ->
    SomeAction monad response

instance (Functor monad) => Functor (SomeAction monad) where
  fmap f (SomeAction v pt a) =
    SomeAction v pt (mapActionResponse f a)

aroundSomeAction ::
  (forall arg. (arg -> monad response) -> arg -> monad' response') ->
  SomeAction monad response ->
  SomeAction monad' response'
aroundSomeAction f (SomeAction v pt (Act a)) =
  SomeAction v pt (Act $ f a)

mapSomeActionResponseMonadic ::
  (Monad monad) =>
  (response1 -> monad response2) ->
  SomeAction monad response1 ->
  SomeAction monad response2
mapSomeActionResponseMonadic f (SomeAction v pt (Act a)) =
  SomeAction
    v
    pt
    ( Act $ \arg -> do
        resp <- a arg
        f resp
    )

hoistSomeActionMonad :: (forall a. m a -> n a) -> SomeAction m response -> SomeAction n response
hoistSomeActionMonad f (SomeAction v pt a) = SomeAction v pt (hoistActionMonad f a)
