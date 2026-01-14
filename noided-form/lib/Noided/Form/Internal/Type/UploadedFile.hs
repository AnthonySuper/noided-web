{-# LANGUAGE NoFieldSelectors #-}

module Noided.Form.Internal.Type.UploadedFile where

import Control.DeepSeq
import Data.Text (Text)
import GHC.Generics

-- | A file uploaded by a user.
data UploadedFile
  = MkUploadedFile
  { -- | User provided MIME type.
    -- Note that the user may be lying.
    mimeType :: !Text,
    -- | User provided file name.
    fileName :: !Text,
    -- | Temporary path the file is stored in.
    -- For simplicity's sake, all uploaded files get stored onto the local file system first.
    -- Modern NVME drives are fast enough that the added latency vs uploading-to-memory should be acceptable.
    tempPath :: !FilePath
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData UploadedFile
