{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.RequestId where

import Data.UUID
import Data.UUID.V4
import Effectful
import Effectful.Dispatch.Dynamic

-- | Effect providing access to a unique ID per request.
data RequestId :: Effect where
  GetRequestId :: RequestId m UUID

type instance DispatchOf RequestId = Dynamic

-- | Read the current request id.
getRequestId :: (RequestId :> es) => Eff es UUID
getRequestId = send GetRequestId

-- | Run with a unique request id for the inner block.
runUniqueRequestId :: (IOE :> es) => Eff (RequestId : es) b -> Eff es b
runUniqueRequestId act = do
  rid <- liftIO nextRandom
  interpret (\_ GetRequestId -> return rid) act
