{-# LANGUAGE OverloadedRecordDot #-}

module Noided.Web.ApplicationConfig
  ( ApplicationConfig (..),
    ConfigurationReadFailedError (..),
    readConfigurationForEnv,
    readConfiguration,
    withInterpretersFromConfig,
  )
where

import Control.Arrow
import Data.Pool qualified as Pool
import Effectful
import Hasql.Connection
import Noided.Web.Internal.Effect.Log
import Noided.Web.Internal.Effect.Translate
import Noided.Web.Internal.Effect.UseConnection
import Noided.Web.Internal.Type.ApplicationConfig
import Noided.Web.Internal.Type.DBSettings
import Noided.Web.Internal.Type.Logger
import Noided.Web.Internal.Type.ServerEnv

import System.IO (stdout)

-- | Use an application config to gain access to an interpreter with some common effects.
withInterpretersFromConfig ::
  ApplicationConfig ->
  ((forall a es. (IOE :> es) => Eff (HasTranslations : UseConnection : Log : es) a -> Eff es a) -> IO b) ->
  IO b
withInterpretersFromConfig cfg cb = do
  pool <- configToPool cfg
  withLoggerFromConfig cfg $ \logger -> do
    translationRunnerFromConfig cfg logger $ \runTranslations ->
      cb
        ( runTranslations
            >>> runUsingConnectionPool pool
            >>> runUsingLogger logger
        )

translationRunnerFromConfig ::
  ApplicationConfig ->
  Logger ->
  ((forall a es. (IOE :> es, Log :> es) => Eff (HasTranslations : es) a -> Eff es a) -> IO b) ->
  IO b
translationRunnerFromConfig cfg logger cb = do
  case cfg.serverEnv of
    Development ->
      cb (runTranslationsFromFile "config/translations")
    _ -> do
      res <- runEff (runUsingLogger logger $ loadTranslations "config/translations")
      cb (runStaticTranslations res)

configToPool :: ApplicationConfig -> IO (Pool.Pool Connection)
configToPool cfg =
  Pool.newPool $
    Pool.defaultPoolConfig
      (either (error . show) id <$> acquire (getHasqlSettings cfg.dbSettings))
      release
      30.0 -- Keep resources for 30 seconds
      10 -- 10 resources per stripe

-- | Use a logger from a configuration.
-- In @ Development @ mode, logs pretty to stdout.
-- In @ Test @ mode, logs are ignored entirely.
-- In @ Production @ mode, JSON logs are output to stdout.
withLoggerFromConfig :: ApplicationConfig -> (Logger -> IO b) -> IO b
withLoggerFromConfig cfg cb = case cfg.serverEnv of
  Development -> withDebugLogger cb
  Test -> cb nullLogger
  Production -> withProductionLoggerToHandle stdout cb
