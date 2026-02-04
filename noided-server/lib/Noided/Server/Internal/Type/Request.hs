{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Server.Internal.Type.Request where

import Data.ByteString (ByteString)
import Data.Kind
import Data.Map.Strict qualified as Map
import GHC.Generics
import Network.HTTP.Types.Header
import Network.Socket (SockAddr)
import Noided.Form.Types
import Noided.Pathname

type Request :: [Type] -> Type
data Request pathParams
  = MkRequest
  { urlParams :: RouteParams pathParams,
    bodyParams :: SomeFormSubmission,
    queryParams :: FormSubmission UrlEncoded,
    headers :: Map.Map HeaderName ByteString,
    remoteHost :: SockAddr
  }
  deriving (Generic)

deriving instance
  (Show (RouteParams params)) =>
  Show (Request params)
