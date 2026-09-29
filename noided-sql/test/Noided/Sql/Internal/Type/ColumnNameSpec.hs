{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Noided.Sql.Internal.Type.ColumnNameSpec (spec) where

import Test.Hspec
import Noided.Sql.Internal.Type.ColumnName
import Data.HKD
import GHC.Generics

data TestRecord f = TestRecord
  { colA :: f ()
  , colB :: f ()
  , colC :: f ()
  , colD :: f ()
  } deriving (Generic)

instance FFunctor TestRecord where
  ffmap = ffmapDefault

instance FFoldable TestRecord where
  ffoldMap = ffoldMapDefault

instance FTraversable TestRecord where
  ftraverse = gftraverse

spec :: Spec
spec = do
  describe "toUniqueNames" $ do
    it "deduplicates repeated column names" $ do
      let input = TestRecord
            { colA = "id"
            , colB = "name"
            , colC = "id"
            , colD = "id"
            } :: TestRecord ColumnName
      
      let result = toUniqueNames input
      
      getUniqueColumnName (colA result) `shouldBe` "\"id\""
      getUniqueColumnName (colB result) `shouldBe` "\"name\""
      getUniqueColumnName (colC result) `shouldBe` "\"id_1\""
      getUniqueColumnName (colD result) `shouldBe` "\"id_2\""

    it "does not collide with names that already look suffixed" $ do
      let input = TestRecord
            { colA = "a"
            , colB = "a"
            , colC = "a_1"
            , colD = "a"
            } :: TestRecord ColumnName
      let result = toUniqueNames input
      getUniqueColumnName (colA result) `shouldBe` "\"a\""
      getUniqueColumnName (colB result) `shouldBe` "\"a_1\""
      getUniqueColumnName (colC result) `shouldBe` "\"a_1_1\""
      getUniqueColumnName (colD result) `shouldBe` "\"a_2\""

    it "does not collide when the suffixed name comes first" $ do
      let input = TestRecord
            { colA = "a_1"
            , colB = "a"
            , colC = "a"
            , colD = "a"
            } :: TestRecord ColumnName
      let result = toUniqueNames input
      getUniqueColumnName (colA result) `shouldBe` "\"a_1\""
      getUniqueColumnName (colB result) `shouldBe` "\"a\""
      getUniqueColumnName (colC result) `shouldBe` "\"a_2\""
      getUniqueColumnName (colD result) `shouldBe` "\"a_3\""
