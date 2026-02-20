{-# LANGUAGE LambdaCase #-}

module Noided.Web.Internal.Type.ServerEnv where

import GHC.Generics

data ServerEnv = Test | Development | Production
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | Singleton type for server environments.
data ServerEnvSing (s :: ServerEnv) where
  TestS :: ServerEnvSing Test
  DevelopmentS :: ServerEnvSing Development
  ProductionS :: ServerEnvSing Production

class KnownServerEnv (serverEnv :: ServerEnv) where
  knownServerEnv :: ServerEnvSing serverEnv

instance KnownServerEnv Test where
  knownServerEnv = TestS

instance KnownServerEnv Development where
  knownServerEnv = DevelopmentS

instance KnownServerEnv Production where
  knownServerEnv = ProductionS

withServerEnv ::
  ServerEnv ->
  (forall serverEnv. ServerEnvSing serverEnv -> a) ->
  a
withServerEnv sv f = case sv of
  Test -> f TestS
  Development -> f DevelopmentS
  Production -> f ProductionS
