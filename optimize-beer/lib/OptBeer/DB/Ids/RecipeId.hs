module OptBeer.DB.Ids.RecipeId where

import Data.Int (Int64)
import GHC.Generics
import Noided.Form
import Noided.Sql.Define
import Web.HttpApiData
import Data.Aeson (ToJSON, FromJSON)

newtype RecipeId = MkRecipeId {getRecipeId :: Int64}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (PGType, FromHttpApiData, ToHttpApiData, FromFormSubmission ct, ToJSON, FromJSON) via Int64

instance AsBindParam RecipeId where
  bindParamEncoder = bindParamEncoderNewtype @Int64

instance AsHaskellValue RecipeId where
  type HaskellTypeOf RecipeId = RecipeId
  decodeHaskellValue = decodeNewtypeWrapper @Int64
