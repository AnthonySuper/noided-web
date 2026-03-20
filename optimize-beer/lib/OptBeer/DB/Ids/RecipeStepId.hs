module OptBeer.DB.Ids.RecipeStepId where

import Data.Int (Int64)
import GHC.Generics
import Noided.Form
import Noided.Sql.Define
import Web.HttpApiData
import Data.Aeson (ToJSON, FromJSON)

newtype RecipeStepId = MkRecipeStepId {getRecipeStepId :: Int64}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (PGType, FromHttpApiData, ToHttpApiData, FromFormSubmission ct, ToJSON, FromJSON) via Int64

instance AsBindParam RecipeStepId where
  bindParamEncoder = bindParamEncoderNewtype @Int64

instance AsHaskellValue RecipeStepId where
  type HaskellTypeOf RecipeStepId = RecipeStepId
  decodeHaskellValue = decodeNewtypeWrapper @Int64
