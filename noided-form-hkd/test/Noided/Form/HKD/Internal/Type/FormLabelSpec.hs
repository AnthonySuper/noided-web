{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Form.HKD.Internal.Type.FormLabelSpec where

import Noided.Form.HKD.Internal.Type.FormLabel
import Noided.Form.HKD.Internal.Type.HKDFieldType (HKDFieldType (InputField, ListField, SubformField))
import Test.Hspec

-- Simple subform for testing
newtype MySubform f = MySubform (f (InputField Int))

deriving instance (Show (f (InputField Int))) => Show (MySubform f)

deriving instance (Eq (f (InputField Int))) => Eq (MySubform f)

spec :: Spec
spec = do
  describe "FormLabel" $ do
    it "shows InputLabel correctly" $ do
      let label = InputLabel "myLabel" :: FormLabel (InputField Int)
      show label `shouldBe` "InputLabel \"myLabel\""

    it "shows SubformLabel correctly" $ do
      let subContent = MySubform (InputLabel "inner")
      let label = SubformLabel "mySub" subContent :: FormLabel (SubformField MySubform)
      show label `shouldBe` "SubformLabel \"mySub\" (MySubform (InputLabel \"inner\"))"

    it "shows ListLabel correctly" $ do
      let inner = InputLabel "elem" :: FormLabel (InputField Int)
      let label = ListLabel "myList" inner :: FormLabel (ListField (InputField Int))
      show label `shouldBe` "ListLabel \"myList\" (InputLabel \"elem\")"
