module OptBeer.DB.Ids.SessionId where

import Data.Int (Int64)
import GHC.Generics
import Noided.Form
import Noided.Sql.Define
import Web.HttpApiData

newtype SessionId = MkSessionId {getSessionId :: Int64}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (PGType, FromHttpApiData, ToHttpApiData, FromFormSubmission ct) via Int64

instance AsBindParam SessionId where
  bindParamEncoder = bindParamEncoderNewtype @Int64

instance AsHaskellValue SessionId where
  type HaskellTypeOf SessionId = SessionId
  decodeHaskellValue = decodeNewtypeWrapper @Int64
