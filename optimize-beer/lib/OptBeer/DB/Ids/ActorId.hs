module OptBeer.DB.Ids.ActorId where

import Data.Int (Int64)
import GHC.Generics
import Noided.Form
import Noided.Sql.Define
import Web.HttpApiData

newtype ActorId = MkActorId {getActorId :: Int64}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (PGType, FromHttpApiData, ToHttpApiData, FromFormSubmission ct) via Int64

instance AsBindParam ActorId where
  bindParamEncoder = bindParamEncoderNewtype @Int64

instance AsHaskellValue ActorId where
  type HaskellTypeOf ActorId = ActorId
  decodeHaskellValue = decodeNewtypeWrapper @Int64
