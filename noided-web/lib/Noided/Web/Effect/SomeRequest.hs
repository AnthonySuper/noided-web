module Noided.Web.Effect.SomeRequest
  ( -- * Base request access
    SomeRequest,
    someRequest,
    runWithSomeRequest,

    -- * Specialized accessors
    GetRequestBody,
    getRequestBody,
    runWithRequestBody,
    runWithBodyFromRequest,
    RequestBody (..),

    -- * Query parameters
    GetQueryParams,
    getQueryParams,
    runWithQueryParams,
    runWithQueryParamsFromRequest,

    -- * Headers
    GetHeaders,
    getHeaders,
    runWithHeaders,
    runWithHeadersFromRequest,

    -- * Remote IP
    GetRemoteIp,
    getRemoteIp,
    runWithRemoteIp,
    runWithRemoteIpFromRequest,
  )
where

import Noided.Server.Internal.Type.Request
import Noided.Web.Internal.Effect.SomeRequest

