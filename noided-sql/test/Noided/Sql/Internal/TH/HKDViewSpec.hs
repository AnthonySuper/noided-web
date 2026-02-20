{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.TH.HKDViewSpec (spec) where

import Data.Functor.Const
import Data.HKD
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.TH.HKDView
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.HaskellT
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.ViewColumnar
import Test.Hspec
import Prelude hiding (id)

-- Data type used to test view template haskell helpers.
data UserViewF realm f
  = UserView
  { id :: ViewColumnar (NonNullT Int64) realm f,
    firstName :: ViewColumnar (NonNullT Text) realm f,
    middleName :: ViewColumnar (NullableT Text) realm f,
    lastName :: ViewColumnar (NonNullT Text) realm f
  }
  deriving (Generic)

$(defineHKDView ''UserViewF)

deriving instance Eq UserView

deriving instance Show UserView

deriving instance Eq UserViewNullified

deriving instance Show UserViewNullified

shouldNameViewColumns :: (FFoldable t) => t ColumnName -> Expectation
shouldNameViewColumns td =
  ffoldMap (pure . getColumnName) td `shouldBe` ["id", "firstName", "middleName", "lastName"]

spec :: Spec
spec = do
  describe "UserViewInQuery" $ do
    it "has a good number of columns" $
      flength (frepeat (Const "") :: UserViewInQuery (Const Text)) `shouldBe` 4
    it "has good named columns" $
      shouldNameViewColumns (namedColumns :: UserViewInQuery ColumnName)
    it "can unwrap a select list" $ do
      let sl :: UserViewInQuery HaskellT
          sl = UserView (HaskT 1) (HaskT "bob") (HaskT Nothing) (HaskT "smith")
      unwrapSelectList sl `shouldBe` UserView 1 "bob" Nothing "smith"
  describe "UserViewNullifiedInQuery" $ do
    it "has a good number of columns" $
      flength (frepeat (Const "") :: UserViewNullifiedInQuery (Const Text)) `shouldBe` 4
    it "has good named columns" $
      shouldNameViewColumns (namedColumns :: UserViewNullifiedInQuery ColumnName)
    it "can unwrap a select list" $ do
      let sl :: UserViewNullifiedInQuery HaskellT
          sl = UserView (HaskT Nothing) (HaskT Nothing) (HaskT Nothing) (HaskT Nothing)
      unwrapSelectList sl `shouldBe` UserView Nothing Nothing Nothing Nothing
  describe "UserView" $ do
    it "has a good Eq instance" $
      (UserView 1 "bob" Nothing "smith" :: UserView)
        `shouldBe` UserView 1 "bob" Nothing "smith"
  describe "UserViewNullified" $ do
    it "has a good Eq instance" $
      (UserView Nothing Nothing Nothing Nothing :: UserViewNullified)
        `shouldBe` UserView Nothing Nothing Nothing Nothing
  return ()
