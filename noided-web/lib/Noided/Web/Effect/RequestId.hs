module Noided.Web.Effect.RequestId
  ( RequestId,
    getRequestId,

    -- ** Interpreters
    runUniqueRequestId,
  )
where

import Noided.Web.Internal.Effect.RequestId
