{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.SomeRequest where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Some.Newtype
import Effectful
import Effectful.Dispatch.Static
import Network.HTTP.Types.Header
import Network.Socket (SockAddr)
import Noided.Form
import Noided.Server

-- | Effect for reading off a request with some unknown path parameters.
--
-- This is used to make actions as generic as possible.
data SomeRequest :: Effect

type instance DispatchOf SomeRequest = Static NoSideEffects

newtype instance StaticRep SomeRequest = SomeRequest (Some Request)

-- | Read off some request.
--
-- You generally should not use this yourself.
-- In order to make testing easier
someRequest :: (SomeRequest :> es) => Eff es (Some Request)
someRequest = do
  (SomeRequest sr) <- getStaticRep
  return sr

-- | Interpret this effect with some request.
runWithSomeRequest :: Some Request -> Eff (SomeRequest : es) a -> Eff es a
runWithSomeRequest = evalStaticRep . SomeRequest

-- | Effect for getting a request body.
data GetRequestBody :: Effect

type instance DispatchOf GetRequestBody = Static NoSideEffects

newtype instance StaticRep GetRequestBody = GRBody RequestBody

getRequestBody :: (GetRequestBody :> es) => Eff es RequestBody
getRequestBody = do
  (GRBody body) <- getStaticRep
  return body

runWithRequestBody :: RequestBody -> Eff (GetRequestBody : es) a -> Eff es a
runWithRequestBody = evalStaticRep . GRBody

runWithBodyFromRequest :: (SomeRequest :> es) => Eff (GetRequestBody : es) b -> Eff es b
runWithBodyFromRequest act = do
  r <- someRequest
  withSome r (\rr -> runWithRequestBody rr.body act)

-- | Effect for reading query params.
data GetQueryParams :: Effect

type instance DispatchOf GetQueryParams = Static NoSideEffects

newtype instance StaticRep GetQueryParams = GQueryParams (FormSubmission UrlEncoded)

-- | Get the query params from the request.
getQueryParams :: (GetQueryParams :> es) => Eff es (FormSubmission UrlEncoded)
getQueryParams = do
  (GQueryParams q) <- getStaticRep
  return q

-- | Run with given query params.
--
-- This is useful when running actions in tests, where you might not want to have to construct an entire request.
runWithQueryParams :: FormSubmission UrlEncoded -> Eff (GetQueryParams : es) a -> Eff es a
runWithQueryParams = evalStaticRep . GQueryParams

-- | Run with the actual query params from a request.
runWithQueryParamsFromRequest :: (SomeRequest :> es) => Eff (GetQueryParams : es) b -> Eff es b
runWithQueryParamsFromRequest act = do
  r <- someRequest
  withSome r (\rr -> runWithQueryParams rr.queryParams act)

data GetHeaders :: Effect

type instance DispatchOf GetHeaders = Static NoSideEffects

newtype instance StaticRep GetHeaders = GHeaders (Map.Map HeaderName ByteString)

getHeaders :: (GetHeaders :> es) => Eff es (Map.Map HeaderName ByteString)
getHeaders = do
  (GHeaders h) <- getStaticRep
  return h

runWithHeaders :: Map.Map HeaderName ByteString -> Eff (GetHeaders : es) a -> Eff es a
runWithHeaders = evalStaticRep . GHeaders

runWithHeadersFromRequest :: (SomeRequest :> es) => Eff (GetHeaders : es) b -> Eff es b
runWithHeadersFromRequest act = do
  r <- someRequest
  withSome r (\rr -> runWithHeaders rr.headers act)

-- | Effect for reading the remote IP of a request.
data GetRemoteIp :: Effect

type instance DispatchOf GetRemoteIp = Static NoSideEffects

newtype instance StaticRep GetRemoteIp = GRemoteIp SockAddr

-- | Get the remote host address of the request.
getRemoteIp :: (GetRemoteIp :> es) => Eff es SockAddr
getRemoteIp = do
  (GRemoteIp ip) <- getStaticRep
  return ip

-- | Run with a given remote host address.
--
-- This is useful when running actions in tests, where you might not want to have to construct an entire request.
runWithRemoteIp :: SockAddr -> Eff (GetRemoteIp : es) a -> Eff es a
runWithRemoteIp = evalStaticRep . GRemoteIp

-- | Run with the actual remote host address from a request.
runWithRemoteIpFromRequest :: (SomeRequest :> es) => Eff (GetRemoteIp : es) b -> Eff es b
runWithRemoteIpFromRequest act = do
  r <- someRequest
  withSome r (\rr -> runWithRemoteIp rr.remoteHost act)
