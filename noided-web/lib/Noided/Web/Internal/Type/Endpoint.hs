{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Type.Endpoint where

import Control.Arrow
import Data.Dependent.Map qualified as DMap
import Data.Foldable
import Effectful
import Effectful.Error.Static
import Network.HTTP.Media
import Network.HTTP.Types (StdMethod (GET))
import Noided.Pathname (PathTemplate)
import Noided.Server
import Noided.Server.Internal.Type.Server
import Noided.Server.Internal.Type.VerbRouter qualified as VR
import Noided.Web.Internal.Type.Response
import Noided.Web.Internal.Type.ServerError
import Optics.Core
import Type.Reflection

-- | An endpoint either returns a 'Response', or a dynamically-typed 'SomeServerError'.
-- This makes handling \"unrecoverable\" errors much easier - you can just define top-level handlers,
-- and things will just work.
type EndpointResponse = Either SomeServerError Response

-- | Type of an endpoint action.
type EndpointAction monad pathParams = Request pathParams -> monad EndpointResponse

newtype Endpoint monad pathParams = MkEndpoint {endpointRoutes :: [(MediaType, EndpointAction monad pathParams)]}
  deriving newtype (Semigroup, Monoid)

_EmptyEndpoint :: Prism (Endpoint monad pathParams) (Endpoint monad pathParams) () b
_EmptyEndpoint = prism' (const mempty) $ \a ->
  case a of
    MkEndpoint [] -> Just ()
    _ -> Nothing

data SomeEndpoint monad where
  SomeEndpoint ::
    StdMethod ->
    PathTemplate pathParams ->
    Endpoint monad pathParams ->
    SomeEndpoint monad

-- | Generic endpoints, that do not render pages or JSON or anything.
--
-- Useful if you want to add some kinds of /generic/ API endpoints,
-- like rendering preview images or something.
newtype SomeEndpoints monad
  = MkSomeEndpoints {getSomeEndpoints :: [SomeEndpoint monad]}
  deriving newtype (Semigroup, Monoid)

-- | Cleans up 'SomeEndpoints' values by merging endpoints with the same method and path template.
cleanupSomeEndpoints :: forall monad. SomeEndpoints monad -> SomeEndpoints monad
cleanupSomeEndpoints =
  getSomeEndpoints
    >>> foldl' f DMap.empty
    >>> DMap.foldrWithKey back []
    >>> MkSomeEndpoints
  where
    back :: forall v. PathTemplate v -> VerbRouterOf (Endpoint monad) v -> [SomeEndpoint monad] -> [SomeEndpoint monad]
    back pt (MkVerbRouterOf vr) base = VR.foldrWithKey (\verb ep -> (SomeEndpoint verb pt ep :)) base vr
    f :: DMap.DMap PathTemplate (VerbRouterOf (Endpoint monad)) -> SomeEndpoint monad -> DMap.DMap PathTemplate (VerbRouterOf (Endpoint monad))
    f dm (SomeEndpoint meth pt ep) =
      dm
        & lensVL (DMap.alterF pt)
        % non' _EmptyVerbRouterOf
        % at meth
        % non' _EmptyEndpoint
        %~ (<> ep)

aroundSomeEndpointActions :: (forall pathParams. EndpointAction monad pathParams -> EndpointAction monad' pathParams) -> SomeEndpoints monad -> SomeEndpoints monad'
aroundSomeEndpointActions f (MkSomeEndpoints eps) = MkSomeEndpoints $ fmap (aroundSomeEndpoint f) eps
  where
    aroundSomeEndpoint :: (forall pathParams. EndpointAction monad pathParams -> EndpointAction monad' pathParams) -> SomeEndpoint monad -> SomeEndpoint monad'
    aroundSomeEndpoint f' (SomeEndpoint method pt ep) = SomeEndpoint method pt (aroundEndpoint f' ep)

someEndpointsHandleAsServerError' ::
  forall err es.
  (Typeable err) =>
  (err -> String) ->
  SomeEndpoints (Eff (Error err : es)) ->
  SomeEndpoints (Eff es)
someEndpointsHandleAsServerError' display = aroundSomeEndpointActions transform
  where
    transform :: forall pathParams. EndpointAction (Eff (Error err : es)) pathParams -> EndpointAction (Eff es) pathParams
    transform action request = do
      res <- runError (action request)
      case res of
        Left (cs, err) -> pure . Left $ SomeServerError (display err) (Just cs) err
        Right endpointRes -> pure endpointRes

someEndpointsHandleAsServerError ::
  forall err es.
  (Typeable err, Show err) =>
  SomeEndpoints (Eff (Error err : es)) ->
  SomeEndpoints (Eff es)
someEndpointsHandleAsServerError = someEndpointsHandleAsServerError' show

endpointOf ::
  StdMethod ->
  PathTemplate pathParams ->
  [(MediaType, EndpointAction monad pathParams)] ->
  SomeEndpoints monad
endpointOf method pt acts =
  MkSomeEndpoints
    [SomeEndpoint method pt (MkEndpoint acts)]

-- | A GET endpoint that returns in the given path.
endpointGet ::
  PathTemplate pathParams ->
  [(MediaType, EndpointAction monad pathParams)] ->
  SomeEndpoints monad
endpointGet = endpointOf GET

-- | Run a function around an entire endpoint.
aroundEndpoint :: (EndpointAction monad pathParams -> EndpointAction monad' pathParams) -> Endpoint monad pathParams -> Endpoint monad' pathParams
aroundEndpoint f (MkEndpoint routes) = MkEndpoint $ (fmap . fmap) f routes

transformResponse ::
  (monad EndpointResponse -> monad' EndpointResponse) ->
  Endpoint monad pathParams ->
  Endpoint monad' pathParams
transformResponse f = aroundEndpoint (\action request -> f (action request))

-- | Handle an 'Effectful.Error.Static.Error' as a server error.
endpointHandleAsServerError' ::
  forall err es pathParams.
  (Typeable err) =>
  (err -> String) ->
  Endpoint (Eff (Error err : es)) pathParams ->
  Endpoint (Eff es) pathParams
endpointHandleAsServerError' display = aroundEndpoint transform
  where
    transform action request = do
      res <- runError (action request)
      case res of
        Left (cs, err) -> pure . Left $ SomeServerError (display err) (Just cs) err
        Right endpointRes -> pure endpointRes

endpointHandleAsServerError ::
  forall err es pathParams.
  (Show err, Typeable err) =>
  Endpoint (Eff (Error err : es)) pathParams ->
  Endpoint (Eff es) pathParams
endpointHandleAsServerError = endpointHandleAsServerError' show
