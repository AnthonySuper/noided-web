module Noided.Web.Internal.Type.Logger
  ( Logger (logMessage),
    withAsyncLoggerOfSize,
    withProductionLoggerToHandle,
    withDebugLogger,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Function
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Noided.Web.Internal.Effect.Log
import Numeric.Natural
import System.IO

-- | A logger can log messages in an IO context.
newtype Logger = MkLogger {logMessage :: LoggedMessage -> IO ()}

buildOutputTo ::
  (LoggedMessage -> Maybe a) ->
  TBQueue a ->
  TVar Bool ->
  LoggedMessage ->
  IO ()
buildOutputTo fmtMsg queue stopVar lm = do
  case fmtMsg lm of
    Nothing -> return ()
    Just a -> do
      aForced <- evaluate a
      atomically $ do
        isDead <- readTVar stopVar
        unless isDead $
          writeTBQueue queue aForced

loggingThread ::
  (a -> IO ()) ->
  TBQueue a ->
  TVar Bool ->
  IO ()
loggingThread outputMsg queue stopVar = fix $ \again -> do
  res <- atomically $ do
    isDead <- readTVar stopVar
    if isDead
      then return Nothing
      else Just <$> readTBQueue queue
  case res of
    Just a -> do
      outputMsg a
      again
    Nothing -> return ()

withAsyncLoggerOfSize ::
  -- | Size of the bounded queue
  Natural ->
  -- | Format a logged message to some other data type.
  -- Note that this value will be reduced to WHNF in the *current thread*.
  (LoggedMessage -> Maybe a) ->
  -- | Output some other data type (probably to a file?)
  (a -> IO ()) ->
  -- | Callback, which uses a logger
  (Logger -> IO b) ->
  -- | Result
  IO b
withAsyncLoggerOfSize sze fmtMsg outputMsg useLogger = do
  queue <- newTBQueueIO sze
  stopVar <- newTVarIO False
  _ <- forkIO $ loggingThread outputMsg queue stopVar
  let logger = MkLogger $ buildOutputTo fmtMsg queue stopVar
  useLogger logger `finally` atomically (writeTVar stopVar True)

-- | Use a logger suitable for production.
-- This logger filters out any `Debug` messages, and outputs JSON-formatted logs to STDOUT
withProductionLoggerToHandle :: Handle -> (Logger -> IO b) -> IO b
withProductionLoggerToHandle h = withAsyncLoggerOfSize 1024 fmtMsg outputMsg
  where
    fmtMsg lm
      | lm.level == Debug = Nothing
      | otherwise = Just $ Aeson.encode lm
    outputMsg bs = LBS.hPut h bs >> hPutChar h '\n' >> hFlush h

-- | Use a logger suitable for Debug operations.
-- This logger will log messages to `stdout`, in the form:
--
--
-- >  [LEVEL] Message
-- >    - Context key: Context Value
-- >    - Context key: Context Value
-- >
--
-- The level will be colorized for ease of readability.
withDebugLogger :: (Logger -> IO b) -> IO b
withDebugLogger = withAsyncLoggerOfSize 1024 (Just . formatMsg) putStr
  where
    formatMsg lm =
      let lvlStr = colorize lm.level ("[" <> show lm.level <> "]")
          header = lvlStr <> " " <> T.unpack lm.msg
          ctxLines = Map.toList lm.ctx <&> \(k, v) -> "  - " <> T.unpack k <> ": " <> formatValue v
       in unlines (header : ctxLines)

    formatValue (Aeson.String t) = T.unpack t
    formatValue v = T.unpack $ T.decodeUtf8 $ LBS.toStrict $ Aeson.encode v

    colorize Debug s = "\ESC[34m" <> s <> "\ESC[0m"
    colorize Info s = "\ESC[32m" <> s <> "\ESC[0m"
    colorize Warn s = "\ESC[33m" <> s <> "\ESC[0m"
    colorize Error s = "\ESC[31m" <> s <> "\ESC[0m"
    colorize Fatal s = "\ESC[35m" <> s <> "\ESC[0m"
