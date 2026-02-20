{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Sql.Internal.HKDTableDefSpec (spec) where

import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics
import Noided.Row (HKDToRow (..))
import Noided.Sql.Define
import Noided.Sql.Internal.HKDTableDef
import Noided.Sql.Internal.Type.ColumnName (ColumnName (..))
import Test.Hspec

data UserF realm f = User
  { userId :: Columnar (Column NoDefault NonNull Int64) realm f,
    userName :: Columnar (Column NoDefault NonNull Text) realm f,
    userEmailAddress :: Columnar (Column NoDefault Nullable Text) realm f
  }
  deriving (Generic)

$(defineHKDTable ''UserF)

data ProfileF realm f = Profile
  { profileBio :: Columnar (Column NoDefault NonNull Text) realm f,
    profileWebsiteUrl :: Columnar (Column NoDefault Nullable Text) realm f
  }
  deriving (Generic)

$(defineHKDTable ''ProfileF)

data UserWithProfileF realm f = UserWithProfile
  { userId :: Columnar (Column NoDefault NonNull Int64) realm f,
    userName :: Columnar (Column NoDefault NonNull Text) realm f,
    userProfile :: ProfileF realm f
  }
  deriving (Generic)

$(defineHKDTable ''UserWithProfileF)

deriving instance Eq (UserInQuery ColumnName)
deriving instance Show (UserInQuery ColumnName)
deriving instance Eq (UserWithProfileInQuery ColumnName)
deriving instance Show (UserWithProfileInQuery ColumnName)
deriving instance Eq (ProfileInQuery ColumnName)
deriving instance Show (ProfileInQuery ColumnName)

spec :: Spec
spec = do
  describe "hkdTableDef" $ do
    it "generates correct table definition with snake_case columns" $ do
      let def = hkdTableDef @UserF "users"

      tableName (def :: TableDefinition (HKDRowLabels UserTableDef) UserInQuery) `shouldBe` "users"

      let names = selectedNames def
      names
        `shouldBe` ( User
                       { userId = MkColumnName "user_id",
                         userName = MkColumnName "user_name",
                         userEmailAddress = MkColumnName "user_email_address"
                       } ::
                       UserInQuery ColumnName
                   )

    it "generates correct table definition with sub-HKDs" $ do
      let def = hkdTableDef @UserWithProfileF "users_with_profiles"

      let names = selectedNames def
      names
        `shouldBe` ( UserWithProfile
                       { userId = MkColumnName "user_id",
                         userName = MkColumnName "user_name",
                         userProfile = Profile
                           { profileBio = MkColumnName "profile_bio",
                             profileWebsiteUrl = MkColumnName "profile_website_url"
                           }
                       } ::
                       UserWithProfileInQuery ColumnName
                   )
