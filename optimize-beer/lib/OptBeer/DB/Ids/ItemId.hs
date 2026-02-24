module OptBeer.DB.Ids.ItemId where

import Data.Int (Int64)
import GHC.Generics
import Noided.Form
import Noided.Sql.Define
import Web.HttpApiData
import Data.Aeson (ToJSON, FromJSON)

newtype ItemId = MkItemId {getItemId :: Int64}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (PGType, FromHttpApiData, ToHttpApiData, FromFormSubmission ct, ToJSON, FromJSON) via Int64

instance AsBindParam ItemId where
  bindParamEncoder = bindParamEncoderNewtype @Int64

instance AsHaskellValue ItemId where
  type HaskellTypeOf ItemId = ItemId
  decodeHaskellValue = decodeNewtypeWrapper @Int64
