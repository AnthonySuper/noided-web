{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.App where
 
 import Network.Wai qualified as Wai
 import Noided.Web

import Noided.Web.Internal.Type.Application
import OptBeer.Action

useOptBeerApplication :: (Wai.Request -> IO Wai.Response) -> (Wai.Application -> IO b) -> IO b
useOptBeerApplication = useNoidedApplication config
  where
    config = withPages optBeerActions
