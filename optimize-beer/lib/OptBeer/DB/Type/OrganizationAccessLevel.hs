{-# LANGUAGE OverloadedStrings #-}

module OptBeer.DB.Type.OrganizationAccessLevel where

import GHC.Generics
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Noided.Sql.Define

data OrganizationAccessLevel
  = Guest
  | Member
  | Admin
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance PGType OrganizationAccessLevel where
  pgTypeName _ = "organization_access_level"

instance AsBindParam OrganizationAccessLevel where
  bindParamEncoder = EncodeNonNull $ Enc.enum (Just "public") "organization_access_level" $ \case
    Guest -> "guest"
    Member -> "member"
    Admin -> "admin"

instance AsHaskellValue OrganizationAccessLevel where
  type HaskellTypeOf OrganizationAccessLevel = OrganizationAccessLevel
  decodeHaskellValue _ = Dec.enum (Just "public") "organization_access_level" $ \case
    "guest" -> Just Guest
    "member" -> Just Member
    "admin" -> Just Admin
    _ -> Nothing
