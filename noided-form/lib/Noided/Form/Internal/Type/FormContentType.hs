module Noided.Form.Internal.Type.FormContentType where

import GHC.Generics

-- | Data kind used to determine the content type of a form.
data FormContentType
  = -- | URL encoded forms, where each value is a text value.
    UrlEncoded
  | -- | Multipart form data forms, where values may be uploaded files.
    MultipartFormData
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)
