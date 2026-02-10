{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Type.Endpoint where

import Effectful
import Effectful.Error.Static
import Network.HTTP.Media
import Noided.Server
import Noided.Web.Internal.Type.Response
import Noided.Web.Internal.Type.ServerError
import Type.Reflection

-- | And endpoint either returns a 'Response', or a dynamically-typed 'SomeServerError'.
-- This makes handling \"unrecoverable\" errors much easier - you can just define top-level handlers,
-- and things will just work.
type EndpointResponse = Either SomeServerError Response

-- | Type of an endpoint action.
type EndpointAction monad pathParams = Request pathParams -> monad EndpointResponse

newtype Endpoint monad pathParams = MkEndpoint {endpointRoutes :: [(MediaType, EndpointAction monad pathParams)]}
  deriving newtype (Semigroup, Monoid)

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
  (Show err, Typeable err) =>
  Endpoint (Eff (Error err : es)) pathParams ->
  Endpoint (Eff es) pathParams
endpointHandleAsServerError = endpointHandleAsServerError' show
