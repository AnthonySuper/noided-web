module OptBeer.DB.Ids.OrganizationId where

import Data.Int (Int64)
import GHC.Generics
import Noided.Form
import Noided.Sql.Define
import Web.HttpApiData
import Data.Aeson (ToJSON, FromJSON)

newtype OrganizationId = MkOrganizationId {getOrganizationId :: Int64}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (PGType, FromHttpApiData, ToHttpApiData, FromFormSubmission ct, ToJSON, FromJSON) via Int64

instance AsBindParam OrganizationId where
  bindParamEncoder = bindParamEncoderNewtype @Int64

instance AsHaskellValue OrganizationId where
  type HaskellTypeOf OrganizationId = OrganizationId
  decodeHaskellValue = decodeNewtypeWrapper @Int64
