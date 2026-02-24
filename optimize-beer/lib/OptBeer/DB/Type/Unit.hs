{-# LANGUAGE OverloadedStrings #-}

module OptBeer.DB.Type.Unit where

import GHC.Generics
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Noided.Sql.Define

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

instance PGType Unit where
  pgTypeName _ = "unit"

instance AsBindParam Unit where
  bindParamEncoder = EncodeNonNull $ Enc.enum (Just "public") "unit" $ \case
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

instance AsHaskellValue Unit where
  type HaskellTypeOf Unit = Unit
  decodeHaskellValue _ = Dec.enum (Just "public") "unit" $ \case
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
