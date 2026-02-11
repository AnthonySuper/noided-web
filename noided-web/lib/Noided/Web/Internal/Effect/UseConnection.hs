{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.UseConnection where

import Data.Pool qualified as Pool
import Effectful
import Effectful.Dispatch.Dynamic
import Hasql.Connection

data UseConnection :: Effect where
  UseConnection :: (Connection -> m a) -> UseConnection m a

type instance DispatchOf UseConnection = Dynamic

useConnection :: (UseConnection :> es) => (Connection -> Eff es a) -> Eff es a
useConnection f = send (UseConnection f)

runUsingSingleConnection :: Connection -> Eff (UseConnection : es) a -> Eff es a
runUsingSingleConnection c = interpret $ \env (UseConnection cb) ->
  localSeqUnlift env $ \unlift ->
    unlift (cb c)

runUsingConnectionPool :: (IOE :> es) => Pool.Pool Connection -> Eff (UseConnection : es) a -> Eff es a
runUsingConnectionPool connPool = interpret $ \env (UseConnection cb) ->
  localSeqUnliftIO env $ \unlifter ->
    Pool.withResource connPool $ \connection ->
      unlifter (cb connection)
