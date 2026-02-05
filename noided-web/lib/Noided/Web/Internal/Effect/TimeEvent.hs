{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.TimeEvent where

import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import Data.Time (NominalDiffTime, diffUTCTime)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import Effectful.State.Static.Local
import GHC.Generics
import Noided.Web.Internal.Effect.CurrentTime
import Optics.Core

-- | Newtype wrapper for event names.
-- You should use the 'IsString' instance to construct these.
newtype TimedEventName = TimedEventName {getTimedEventName :: Text}
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString TimedEventName where
  fromString = TimedEventName . fromString

-- | Effect for timing different events as the server runs.
data TimeEvent :: Effect where
  RecordEventTime ::
    TimedEventName ->
    m a ->
    TimeEvent m a

type instance DispatchOf TimeEvent = Dynamic

-- | Record the timing of an event.
-- This timing will be used to report metrics.
recordEventTime :: (TimeEvent :> es) => TimedEventName -> Eff es a -> Eff es a
recordEventTime name = send . RecordEventTime name

-- | Ignore event timings.
runIgnoringEventTimings :: Eff (TimeEvent : es) a -> Eff es a
runIgnoringEventTimings = interpret $ \env (RecordEventTime _ act) ->
  localSeqUnlift env $ \lift -> lift act

type TimingMap = Map TimedEventName NominalDiffTime

-- | Record event timings using 'CurrentTime' and return a map of each event to the time it took.
--
-- You can log this map to some metrics system, or possibly use it as the @Server-Timing@ header.
runRecordingEventTimings :: (CurrentTime :> es) => Eff (TimeEvent : es) a -> Eff es (a, TimingMap)
runRecordingEventTimings = reinterpret (runState @TimingMap mempty) $ \env (RecordEventTime t act) -> do
  -- record the duration before here, so we don't double-record an event if
  -- the same event name is nested
  durationBefore <- view (at t % non 0) <$> get @TimingMap
  timeBefore <- getCurrentTime
  (localSeqUnlift env $ \lift -> lift act) `finally` do
    timeAfter <- getCurrentTime
    let elapsedTime = timeBefore `diffUTCTime` timeAfter
    let newDuration = durationBefore + elapsedTime
    modify @TimingMap (at t ?~ newDuration)
