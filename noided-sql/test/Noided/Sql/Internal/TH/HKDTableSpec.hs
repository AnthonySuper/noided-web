{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.TH.HKDTableSpec (spec) where

import Data.Functor.Const
import Data.HKD
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.TH.HKDTable
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.Columnar
import Noided.Sql.Internal.Type.Nullability
import Test.Hspec
import Prelude hiding (id)

-- Data type used to test template haskell helpers.
data UserF realm f
  = User
  { id :: Columnar (Column AlwaysDefault NonNull Int64) realm f,
    firstName :: Columnar (Column NoDefault NonNull Text) realm f,
    middleName :: Columnar (Column NoDefault Nullable Text) realm f,
    lastName :: Columnar (Column NoDefault NonNull Text) realm f
  }
  deriving (Generic)

$(defineHKDTable ''UserF)

deriving instance Eq User

deriving instance Show User

userColumns :: UserTableDef ColumnName
userColumns = User {id = "id", firstName = "first_name", middleName = "middle_name", lastName = "last_name"}

shouldNameColumns :: (FFoldable t) => t ColumnName -> Expectation
shouldNameColumns td =
  ffoldMap (pure . getColumnName) td `shouldBe` ["id", "firstName", "middleName", "lastName"]

spec :: Spec
spec = do
  describe "UserTableDef" $ do
    it "has a good number of columns" $
      flength userColumns `shouldBe` 4
    it "has good named columns" $
      shouldNameColumns (namedColumns :: UserTableDef ColumnName)
  describe "UserInQuery" $ do
    it "has a good number of columns" $
      flength (frepeat (Const "") :: UserInQuery (Const Text)) `shouldBe` 4
    it "has good named columns" $
      shouldNameColumns (namedColumns :: UserInQuery ColumnName)
  describe "UserNullifiedInQuery" $ do
    it "has a good number of columns" $
      flength (frepeat (Const "") :: UserNullifiedInQuery (Const Text)) `shouldBe` 4
    it "has good named columns" $
      shouldNameColumns (namedColumns :: UserNullifiedInQuery ColumnName)
  describe "User" $ do
    it "has a good Eq instance" $
      (User 1 "bob" Nothing "smith" :: User)
        `shouldBe` User 1 "bob" Nothing "smith"
  return ()
