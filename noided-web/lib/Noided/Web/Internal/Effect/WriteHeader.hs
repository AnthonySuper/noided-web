{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.WriteHeader where

import Data.ByteString
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Network.HTTP.Types.Header
import Optics.Core

-- | Effect for writing header values.
data WriteHeader :: Effect where
  WriteHeader ::
    HeaderName ->
    ByteString ->
    WriteHeader m ()

type instance DispatchOf WriteHeader = Dynamic

-- | Write a header to be used in the response eventually.
writeHeader :: (WriteHeader :> es) => HeaderName -> ByteString -> Eff es ()
writeHeader hn = send . WriteHeader hn

type HeaderMap = Map.Map HeaderName ByteString

-- | Run 'WriteHeader', accumulating errors in a map.
runWriteHeaderMap :: Eff (WriteHeader : es) a -> Eff es (a, HeaderMap)
runWriteHeaderMap = reinterpret (runState @HeaderMap mempty) $ \_ (WriteHeader hn bs) ->
  modify @HeaderMap (at hn ?~ bs)

-- | Run 'WriteHeader' ignoring all written headers.
runIgnoringWrittenHeaders :: Eff (WriteHeader : es) a -> Eff es a
runIgnoringWrittenHeaders = interpret $ \_ (WriteHeader _ _) -> return ()
