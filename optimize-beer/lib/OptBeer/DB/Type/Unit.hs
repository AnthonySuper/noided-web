{-# LANGUAGE OverloadedStrings #-}

module OptBeer.DB.Type.Unit where

import Data.Text (Text)
import GHC.Generics
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Noided.Form
import Noided.Sql.Define
import Web.HttpApiData

data Unit
  = Gram
  | Kilogram
  | Ounce
  | Pound
  | Milliliter
  | Liter
  | Hectoliter
  | FluidOunce
  | Gallon
  | UsBeerBarrel
  | Each
  | Minute
  | Hour
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

unitToText :: Unit -> Text
unitToText = \case
  Gram -> "gram"
  Kilogram -> "kilogram"
  Ounce -> "ounce"
  Pound -> "pound"
  Milliliter -> "milliliter"
  Liter -> "liter"
  Hectoliter -> "hectoliter"
  FluidOunce -> "fluid_ounce"
  Gallon -> "gallon"
  UsBeerBarrel -> "us_beer_barrel"
  Each -> "each"
  Minute -> "minute"
  Hour -> "hour"

unitFromText :: Text -> Maybe Unit
unitFromText = \case
  "gram" -> Just Gram
  "kilogram" -> Just Kilogram
  "ounce" -> Just Ounce
  "pound" -> Just Pound
  "milliliter" -> Just Milliliter
  "liter" -> Just Liter
  "hectoliter" -> Just Hectoliter
  "fluid_ounce" -> Just FluidOunce
  "gallon" -> Just Gallon
  "us_beer_barrel" -> Just UsBeerBarrel
  "each" -> Just Each
  "minute" -> Just Minute
  "hour" -> Just Hour
  _ -> Nothing

instance PGType Unit where
  pgTypeName _ = "unit"

instance AsBindParam Unit where
  bindParamEncoder = EncodeNonNull $ Enc.enum (Just "public") "unit" unitToText

instance AsHaskellValue Unit where
  type HaskellTypeOf Unit = Unit
  decodeHaskellValue _ = Dec.enum (Just "public") "unit" unitFromText

instance ToHttpApiData Unit where
  toUrlPiece = unitToText

instance FromHttpApiData Unit where
  parseUrlPiece t = case unitFromText t of
    Just u -> Right u
    Nothing -> Left "Unknown unit"

instance FromFormSubmission ct Unit where
  fromFormSubmission = fmap getViaHttpParam . fromFormSubmission
