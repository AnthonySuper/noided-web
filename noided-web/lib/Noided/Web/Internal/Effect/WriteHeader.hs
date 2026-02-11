{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.WriteHeader where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Network.HTTP.Types.Header

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

type HeaderMap = [(HeaderName, ByteString)]

-- | Run 'WriteHeader', accumulating headers in a map.
runWriteHeaderMap :: Eff (WriteHeader : es) a -> Eff es (a, HeaderMap)
runWriteHeaderMap = fmap (second reverse) . interpretInner
  where
    interpretInner = reinterpret (runState @HeaderMap mempty) $ \_ (WriteHeader hn bs) ->
      modify @HeaderMap ((hn, bs) :)

runIgnoringWrittenHeaders :: Eff (WriteHeader : es) a -> Eff es a
runIgnoringWrittenHeaders = interpret $ \_ (WriteHeader _ _) -> return ()
