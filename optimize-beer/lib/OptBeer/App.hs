{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.App where

import Network.Wai qualified as Wai
import Noided.Web
import OptBeer.Action

useOptBeerApplication :: (Wai.Application -> IO b) -> IO b
useOptBeerApplication = useNoidedApplication config
  where
    config = withPages optBeerActions
