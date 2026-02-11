{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Effect.TimeEvent
  ( TimeEvent,
    TimedEventName (..),
    recordEventTime,
    recordStaticTime,

    -- ** Interpreters
    runIgnoringEventTimings,
    runRecordingEventTimings,
    runLoggingEventTimingsToHeader,
  )
where

import Data.ByteString (ByteString)
import Effectful
import Noided.Web.Internal.Effect.CurrentTime
import Noided.Web.Internal.Effect.TimeEvent
import Noided.Web.Internal.Effect.WriteHeader

renderTimingMapHeader :: TimingMap -> ByteString
renderTimingMapHeader = error "TODO: implement me"

runLoggingEventTimingsToHeader :: (CurrentTime :> es, WriteHeader :> es) => Eff (TimeEvent : es) b -> Eff es b
runLoggingEventTimingsToHeader act = do
  (res, timingMap) <- runRecordingEventTimings act
  writeHeader "Server-Timing" (renderTimingMapHeader timingMap)
  return res
