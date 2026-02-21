{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.ServerEnv where

import Effectful
import Effectful.Dispatch.Dynamic
import Noided.Web.Internal.Type.ServerEnv

data GetServerEnv :: Effect where
  GetServerEnv :: GetServerEnv m ServerEnv

type instance DispatchOf GetServerEnv = Dynamic

getServerEnv :: (GetServerEnv :> es) => Eff es ServerEnv
getServerEnv = send GetServerEnv

runServerEnv :: ServerEnv -> Eff (GetServerEnv : es) a -> Eff es a
runServerEnv env = interpret $ \_ GetServerEnv -> return env
