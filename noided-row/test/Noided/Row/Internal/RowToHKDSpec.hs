{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Row.Internal.RowToHKDSpec (spec) where

import GHC.Generics
import Noided.Row
import Test.Hspec

data PersonT w
  = MkPerson {name :: w String, age :: w Int}
  deriving (Generic)

deriving instance Show (PersonT Maybe)

deriving instance Eq (PersonT Maybe)

basicRow :: WrappedRow ["name" :=> String, "age" :=> Int] Maybe
basicRow =
  #name :==> Just "foo"
    :::%? #age :==> Nothing
    :::%? EmptyWrappedRow

basicPersonT :: PersonT Maybe
basicPersonT = MkPerson (Just "foo") Nothing

spec :: Spec
spec = do
  it "does row to row conversion" $ do
    rowToHKD basicRow `shouldBe` basicRow
  it "converts forward" $ do
    rowToHKD basicRow `shouldBe` basicPersonT
