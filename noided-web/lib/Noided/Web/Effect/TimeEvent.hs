module Noided.Web.Effect.TimeEvent
  ( TimeEvent,
    TimedEventName (..),
    recordEventTime,
    recordStaticTime,

    -- ** Interpreters
    runIgnoringEventTimings,
    runRecordingEventTimings,
  )
where

import Noided.Web.Internal.Effect.TimeEvent
