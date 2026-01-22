{-# LANGUAGE DataKinds #-}

module Noided.Row.Internal.DivideSpec (spec) where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Noided.Row
import Test.Hspec

newtype WriteFields a = WF {runWriteFields :: a -> [String]}

instance Contravariant WriteFields where
  contramap f (WF wf) = WF $ wf . f

instance Divisible WriteFields where
  conquer = WF $ const []
  divide f (WF l) (WF r) = WF $ \a ->
    case f a of
      (lhs, rhs) -> l lhs <> r rhs

writerRow :: WrappedRow ["foo" :=> Int, "bar" :=> Int] WriteFields
writerRow = WF (pure . show) :::% WF (pure . show) :::% EmptyWrappedRow

writerInput :: Row ["foo" :=> Int, "bar" :=> Int]
writerInput = 10 ::: 11 ::: EmptyRow

spec :: Spec
spec = do
  it "can divide an empty row" $
    runWriteFields (rowDivided EmptyWrappedRow) EmptyRow `shouldBe` []
  it "can divide other rows" $
    runWriteFields (rowDivided writerRow) writerInput `shouldBe` ["10", "11"]
  it "can divide big rows" $
    runWriteFields (rowDivided $ writerRow `wrappedRowAppend` writerRow) (writerInput `rowAppend` writerInput)
      `shouldBe` ["10", "11", "10", "11"]
