{-# LANGUAGE DerivingVia #-}

module Noided.Form.HKD.Internal.Type.FormErrorsSpec (spec) where

import Data.HKD
import Data.IntMap qualified as IM
import GHC.Generics
import Noided.Form.HKD.Internal.Type.FormErrors
import Noided.Form.HKD.Internal.Type.HKDFieldType
import Test.Hspec

data PersonForm wrapper
  = PersonF
  { firstName :: wrapper (InputField String),
    lastName :: wrapper (InputField String),
    age :: wrapper (InputField Int)
  }
  deriving (Generic)

instance FFunctor PersonForm where
  ffmap = ffmapDefault

instance FFoldable PersonForm where
  ffoldMap = ffoldMapDefault

instance FTraversable PersonForm where
  ftraverse = gftraverse

instance FZip PersonForm where
  fzipWith = gfzipWith

instance FRepeat PersonForm where
  frepeat = gfrepeat

deriving instance Show (PersonForm FormErrors)

deriving instance Eq (PersonForm FormErrors)

deriving instance Ord (PersonForm FormErrors)

deriving via (Generically (PersonForm FormErrors)) instance Semigroup (PersonForm FormErrors)

deriving via (Generically (PersonForm FormErrors)) instance Monoid (PersonForm FormErrors)

spec :: Spec
spec = do
  describe "eq instances" $ do
    it "works with input errors" $
      InputErrors mempty `shouldBe` mempty
    it "works with subform errors" $
      SubformErrors mempty (mempty :: PersonForm FormErrors) `shouldBe` mempty
    it "works with list errors" $
      ListErrors mempty (IM.empty :: IM.IntMap (FormErrors (InputField Int))) `shouldBe` mempty
  describe "show instace" $ do
    it "works with input errors" $
      show (InputErrors mempty) `shouldBe` "InputErrors (fromList [])"
    it "works with subform errors" $
      show (SubformErrors mempty (mempty :: PersonForm FormErrors))
        `shouldBe` "SubformErrors (fromList []) (PersonF {firstName = InputErrors (fromList []), lastName = InputErrors (fromList []), age = InputErrors (fromList [])})"
