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
  )
where

import Noided.Web.Internal.Effect.SomeRequest
