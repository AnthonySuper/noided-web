{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Server.Internal.Type.Request where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Types.Header
import Network.Socket (SockAddr)
import Noided.Form.Types
import Noided.Pathname

type Request :: [Type] -> Type
data Request pathParams
  = MkRequest
  { urlParams :: RouteParams pathParams,
    body :: RequestBody,
    queryParams :: FormSubmission UrlEncoded,
    headers :: Map.Map HeaderName ByteString,
    remoteHost :: SockAddr
  }
  deriving (Generic)

deriving instance
  (Show (RouteParams params)) =>
  Show (Request params)

-- | The body of a request.
data RequestBody
  = -- | Request had no body.
    NoBody
  | -- | Request had a form body of some variety.
    FormBody SomeFormSubmission
  | -- | Request had a JSON body
    JSONBody Value
  | -- | Request body was malformed (e.g. invalid JSON)
    MalformedBody Text
  | UnknownBody RequestBodyUnknown
  deriving (Generic, Show)

data RequestBodyUnknown
  = ReqBodyUnknown
  { mediaType :: Maybe (ByteString, [(ByteString, ByteString)]),
    readChunk :: IO RequestChunk
  }
  deriving (Generic)

instance Show RequestBodyUnknown where
  show (ReqBodyUnknown m _) = "ReqBodyUnknown { mediaType = " ++ show m ++ ", readChunk = <IO> }"

-- | A chunk of a request
data RequestChunk = ActualChunk ByteString | EndOfInput
  deriving (Show, Read, Eq, Ord, Generic)
