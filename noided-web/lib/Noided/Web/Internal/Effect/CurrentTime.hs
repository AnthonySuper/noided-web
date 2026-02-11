{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.CurrentTime where

import Data.Time (UTCTime)
import Data.Time qualified as Time
import Effectful
import Effectful.Dispatch.Dynamic

-- | Effect for reading the current time.
data CurrentTime :: Effect where
  GetCurrentTime :: CurrentTime m UTCTime

type instance DispatchOf CurrentTime = Dynamic

-- | Get the current time in some effectful monad.
getCurrentTime :: (CurrentTime :> es) => Eff es UTCTime
getCurrentTime = send GetCurrentTime

runCurrentTime :: (IOE :> es) => Eff (CurrentTime : es) a -> Eff es a
runCurrentTime = interpret $ \_ GetCurrentTime -> liftIO Time.getCurrentTime

runStaticTime :: UTCTime -> Eff (CurrentTime : es) a -> Eff es a
runStaticTime t = interpret $ \_ GetCurrentTime -> return t
