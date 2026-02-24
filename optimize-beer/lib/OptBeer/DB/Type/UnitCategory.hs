{-# LANGUAGE OverloadedStrings #-}

module OptBeer.DB.Type.UnitCategory where

import GHC.Generics
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Noided.Sql.Define

data UnitCategory
  = Mass
  | Volume
  | Time
  | Count
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance PGType UnitCategory where
  pgTypeName _ = "unit_category"

instance AsBindParam UnitCategory where
  bindParamEncoder = EncodeNonNull $ Enc.enum (Just "public") "unit_category" $ \case
    Mass -> "mass"
    Volume -> "volume"
    Time -> "time"
    Count -> "count"

instance AsHaskellValue UnitCategory where
  type HaskellTypeOf UnitCategory = UnitCategory
  decodeHaskellValue _ = Dec.enum (Just "public") "unit_category" $ \case
    "mass" -> Just Mass
    "volume" -> Just Volume
    "time" -> Just Time
    "count" -> Just Count
    _ -> Nothing
