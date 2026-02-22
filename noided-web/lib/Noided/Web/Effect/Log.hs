{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Effect.Log
  ( Log,
    logMessage,
    logText,
    LoggedMessage (..),
    LogLevel (..),

    -- * Running loggers
    runIgnoringLogs,
    runLoggingToList,

    -- ** Transformers to add context
    loggingCurrentTime,
    loggingRequestId,
  )
where

import Effectful
import Noided.Web.Internal.Effect.CurrentTime
import Noided.Web.Internal.Effect.Log
import Noided.Web.Internal.Effect.RequestId

loggingCurrentTime :: (Log :> es, CurrentTime :> es) => Eff es a -> Eff es a
loggingCurrentTime = addLoggingContextM "currentTime" getCurrentTime

loggingRequestId :: (Log :> es, RequestId :> es) => Eff es a -> Eff es a
loggingRequestId = addLoggingContextM "requestId" getRequestId
