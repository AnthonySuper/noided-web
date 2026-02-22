module OptBeer.DB.Ids.LoginAttemptId where

import Data.Int (Int64)
import GHC.Generics
import Noided.Form
import Noided.Sql.Define
import Web.HttpApiData

newtype LoginAttemptId = MkLoginAttemptId {getLoginAttemptId :: Int64}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (PGType, FromHttpApiData, ToHttpApiData, FromFormSubmission ct) via Int64

instance AsBindParam LoginAttemptId where
  bindParamEncoder = bindParamEncoderNewtype @Int64

instance AsHaskellValue LoginAttemptId where
  type HaskellTypeOf LoginAttemptId = LoginAttemptId
  decodeHaskellValue = decodeNewtypeWrapper @Int64
