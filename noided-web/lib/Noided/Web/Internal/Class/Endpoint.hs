module Noided.Web.Internal.Class.Endpoint where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Kind
import GHC.Generics
import Noided.Server
import Type.Reflection

-- | An actual response body, which will be rendered by the server.
data ResponseBody
  = ByteStringResponse ByteString
  | LazyByteStringResponse LBS.ByteString
  | BuilderResponse Builder
  deriving (Generic)
