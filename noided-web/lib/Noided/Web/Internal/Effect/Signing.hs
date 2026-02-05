{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.Signing where

import Effectful
import Effectful.Dispatch.Dynamic
import Noided.Web.Internal.Type.SignedMessage

data Signing :: Effect where
  MainSigner :: Signing m Signer
  FallbackSigners :: Signing m [Signer]

type instance DispatchOf Signing = Dynamic

mainSigner :: (Signing :> es) => Eff es Signer
mainSigner = send MainSigner

fallbackSigners :: (Signing :> es) => Eff es [Signer]
fallbackSigners = send FallbackSigners

runWithMainSignerAndFallbacks :: Signer -> [Signer] -> Eff (Signing : es) a -> Eff es a
runWithMainSignerAndFallbacks main fallbacks = interpret $ \_ -> \case
  MainSigner -> return main
  FallbackSigners -> return fallbacks

runWithSingleSigner :: Signer -> Eff (Signing : es) a -> Eff es a
runWithSingleSigner signer = runWithMainSignerAndFallbacks signer []
