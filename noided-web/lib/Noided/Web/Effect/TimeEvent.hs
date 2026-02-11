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
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful
import Noided.Web.Internal.Effect.CurrentTime
import Noided.Web.Internal.Effect.TimeEvent
import Noided.Web.Internal.Effect.WriteHeader

renderTimingMapHeader :: TimingMap -> ByteString
renderTimingMapHeader tm = T.encodeUtf8 $ T.intercalate ", " $ map renderOne (Map.toList tm)
  where
    renderOne (TimedEventName name, dur) =
      let ms = (realToFrac dur :: Double) * 1000
       in name <> ";dur=" <> T.pack (show ms)

runLoggingEventTimingsToHeader :: (CurrentTime :> es, WriteHeader :> es) => Eff (TimeEvent : es) b -> Eff es b
runLoggingEventTimingsToHeader act = do
  (res, timingMap) <- runRecordingEventTimings act
  writeHeader "Server-Timing" (renderTimingMapHeader timingMap)
  return res
