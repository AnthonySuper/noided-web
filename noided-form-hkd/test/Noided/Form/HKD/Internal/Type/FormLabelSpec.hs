{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Form.HKD.Internal.Type.FormLabelSpec where

import Data.HKD
import GHC.Generics
import Noided.Form.HKD.Internal.Type.FormLabel
import Noided.Form.HKD.Internal.Type.HKDFieldType (HKDFieldType (InputField, ListField, SubformField))
import Test.Hspec

-- Simple subform for testing
newtype MySubform f = MySubform (f (InputField Int))
  deriving (Generic)

deriving instance (Show (f (InputField Int))) => Show (MySubform f)

deriving instance (Eq (f (InputField Int))) => Eq (MySubform f)

instance FFunctor MySubform where
  ffmap = ffmapDefault

instance FFoldable MySubform where
  ffoldMap = ffoldMapDefault

instance FTraversable MySubform where
  ftraverse = gftraverse

spec :: Spec
spec = do
  describe "FormLabel" $ do
    it "shows InputLabel correctly" $ do
      let label = FormLabel "myLabel" InputLabelInner :: FormLabel (InputField Int)
      show label `shouldBe` "FormLabel \"myLabel\" InputLabelInner"

    it "shows SubformLabel correctly" $ do
      let subContent = MySubform "inner"
      let label = FormLabel "subform" (SubformLabelInner subContent) :: FormLabel (SubformField MySubform)
      show label `shouldBe` "FormLabel \"subform\" (SubformLabelInner (MySubform (FormLabel \"inner\" InputLabelInner)))"

    it "shows ListLabel correctly" $ do
      let label = FormLabel "myList" (ListLabelInner InputLabelInner) :: FormLabel (ListField (InputField Int))
      show label `shouldBe`
        "FormLabel \"myList\" (ListLabelInner InputLabelInner)"
