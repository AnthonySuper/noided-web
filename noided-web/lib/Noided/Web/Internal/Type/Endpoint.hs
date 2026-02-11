{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Type.Endpoint where

import Data.Dependent.Map qualified as DMap
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
--
-- Internally uses a 'DMap' to efficiently merge endpoints with the same
-- path template and HTTP method.
newtype SomeEndpoints monad
  = MkSomeEndpoints {getSomeEndpointsMap :: DMap.DMap PathTemplate (VerbRouterOf (Endpoint monad))}

-- | Semigroup instance merges endpoints by combining verb routers at the same path.
-- When both sides have an endpoint for the same path and method, their Endpoint values
-- are merged using the Endpoint's Semigroup instance.
instance Semigroup (SomeEndpoints monad) where
  MkSomeEndpoints a <> MkSomeEndpoints b =
    MkSomeEndpoints $ DMap.unionWithKey (\_ (MkVerbRouterOf vr1) (MkVerbRouterOf vr2) -> MkVerbRouterOf (VR.unionWith (<>) vr1 vr2)) a b

-- | Monoid instance with empty endpoint map.
instance Monoid (SomeEndpoints monad) where
  mempty = MkSomeEndpoints DMap.empty

-- | Get the list representation of endpoints.
-- This converts the internal map representation to a list of 'SomeEndpoint' values.
getSomeEndpoints :: SomeEndpoints monad -> [SomeEndpoint monad]
getSomeEndpoints (MkSomeEndpoints dm) =
  DMap.foldrWithKey (\pt (MkVerbRouterOf vr) acc -> VR.foldrWithKey (\verb ep -> (SomeEndpoint verb pt ep :)) acc vr) [] dm

aroundSomeEndpointActions :: (forall pathParams. EndpointAction monad pathParams -> EndpointAction monad' pathParams) -> SomeEndpoints monad -> SomeEndpoints monad'
aroundSomeEndpointActions f (MkSomeEndpoints dm) =
  MkSomeEndpoints $ DMap.map (\(MkVerbRouterOf vr) -> MkVerbRouterOf (fmap (aroundEndpoint f) vr)) dm

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
  MkSomeEndpoints $
    DMap.singleton pt (MkVerbRouterOf $ VR.singleton method (MkEndpoint acts))

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
