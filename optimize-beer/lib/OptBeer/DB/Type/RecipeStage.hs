{-# LANGUAGE OverloadedStrings #-}

module OptBeer.DB.Type.RecipeStage where

import Data.Text (Text)
import GHC.Generics
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Noided.Form
import Noided.Sql.Define
import Web.HttpApiData

data RecipeStage
  = Preparation
  | Mash
  | Sparge
  | Boil
  | Whirlpool
  | Fermentation
  | Conditioning
  | Packaging
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

recipeStageToText :: RecipeStage -> Text
recipeStageToText = \case
  Preparation -> "preparation"
  Mash -> "mash"
  Sparge -> "sparge"
  Boil -> "boil"
  Whirlpool -> "whirlpool"
  Fermentation -> "fermentation"
  Conditioning -> "conditioning"
  Packaging -> "packaging"

recipeStageFromText :: Text -> Maybe RecipeStage
recipeStageFromText = \case
  "preparation" -> Just Preparation
  "mash" -> Just Mash
  "sparge" -> Just Sparge
  "boil" -> Just Boil
  "whirlpool" -> Just Whirlpool
  "fermentation" -> Just Fermentation
  "conditioning" -> Just Conditioning
  "packaging" -> Just Packaging
  _ -> Nothing

instance PGType RecipeStage where
  pgTypeName _ = "recipe_stage"

instance AsBindParam RecipeStage where
  bindParamEncoder = EncodeNonNull $ Enc.enum (Just "public") "recipe_stage" recipeStageToText

instance AsHaskellValue RecipeStage where
  type HaskellTypeOf RecipeStage = RecipeStage
  decodeHaskellValue _ = Dec.enum (Just "public") "recipe_stage" recipeStageFromText

instance ToHttpApiData RecipeStage where
  toUrlPiece = recipeStageToText

instance FromHttpApiData RecipeStage where
  parseUrlPiece t = case recipeStageFromText t of
    Just s -> Right s
    Nothing -> Left "Unknown recipe stage"

instance FromFormSubmission ct RecipeStage where
  fromFormSubmission = fmap getViaHttpParam . fromFormSubmission
