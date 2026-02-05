{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.Log where

import Data.Aeson
import Data.Bifunctor (Bifunctor (second))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import GHC.Generics
import Optics.Core

-- | Standard logging levels.
data LogLevel = Debug | Info | Warn | Error | Fatal
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)
  deriving (ToJSON, FromJSON) via (Generically LogLevel)

-- | A log message, with a given level.
data LoggedMessage = LoggedMessage {level :: LogLevel, msg :: Text, ctx :: Map.Map Text Value}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (Generically LoggedMessage)

-- | An effect for logging messages.
data Log :: Effect where
  LogMessage :: LoggedMessage -> Log m ()

type instance DispatchOf Log = Dynamic

logMessage :: (Log :> es) => LoggedMessage -> Eff es ()
logMessage = send . LogMessage

logText :: (Log :> es) => LogLevel -> Text -> Eff es ()
logText lvl msg = logMessage $ LoggedMessage {level = lvl, msg = msg, ctx = mempty}

-- | Run an action and add the context to each log message that passes a predicate.
addLoggingContextsWhenM :: (Log :> es) => (LoggedMessage -> Bool) -> Eff es (Map.Map Text Value) -> Eff es a -> Eff es a
addLoggingContextsWhenM decide fetchContexts = interpose $ \env op@(LogMessage lm) ->
  if decide lm
    then do
      ctxVal <- fetchContexts
      send $ LogMessage $ lm & #ctx %~ (<> ctxVal)
    else passthrough env op

-- | For each message that is at least the given level, add context items to each log message.
-- The action to fetch the context items will only be ran if the context is to be added.
addLoggingContextsWithLevelM :: (Log :> es) => LogLevel -> Eff es (Map.Map Text Value) -> Eff es a -> Eff es a
addLoggingContextsWithLevelM lvl = addLoggingContextsWhenM (\msg -> msg.level >= lvl)

-- | Add context from an action to each logged message.
addLoggingContextsM :: (Log :> es) => Eff es (Map.Map Text Value) -> Eff es a -> Eff es a
addLoggingContextsM = addLoggingContextsWithLevelM Debug

-- | Add a context item to each log action at or above a given level.
--
-- The action may be from an effect, which will be ran every time a log message is generated.
addLoggingContextWithLevelM :: (Log :> es) => LogLevel -> Text -> Eff es Value -> Eff es a -> Eff es a
addLoggingContextWithLevelM lvl k v = addLoggingContextsWithLevelM lvl (Map.singleton k <$> v)

-- | Add a context item to each logged message.
--
-- The action to fetch the context item will be ran each time a message is logged.
addLoggingContextM :: (Log :> es, ToJSON ctxItem) => Text -> Eff es ctxItem -> Eff es a -> Eff es a
addLoggingContextM k v = addLoggingContextsM (Map.singleton k . toJSON <$> v)

-- | Run ignoring every log.
runIgnoringLogs :: Eff (Log : es) a -> Eff es a
runIgnoringLogs = interpret $ \_ (LogMessage _) -> return ()

-- | Run each logged message into a list.
-- The list will be in order.
-- Internally uses a state monad instead of a writer monad to avoid space leaks.
runLoggingToList :: Eff (Log : es) a -> Eff es (a, [LoggedMessage])
runLoggingToList = fmap (second reverse) . handle
  where
    handle = reinterpret (runState @[LoggedMessage] []) $ \_ (LogMessage m) -> modify @[LoggedMessage] (m :)
