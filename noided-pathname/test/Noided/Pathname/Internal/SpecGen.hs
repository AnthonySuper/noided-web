{-# LANGUAGE OverloadedLists #-}

module Noided.Pathname.Internal.SpecGen (genSomePiece, genSomePath, invertComparison) where

import Data.Some.Newtype
import Data.Text (pack)
import Noided.Pathname.Internal.PathTemplate
import Noided.Pathname.Internal.PieceTemplate
import Test.QuickCheck (Gen, choose, elements, oneof, vectorOf)

genSomePiece :: Gen (Some PieceTemplate)
genSomePiece =
  oneof [genText, genCapture]
  where
    genCapture =
      elements
        [ Some $ capPiece @Int,
          Some $ capPiece @Float,
          Some $ capPiece @String
        ]
    genChar = do
      r <- choose (fromEnum 'a', fromEnum 'z')
      pure $ toEnum r
    genText = do
      len <- choose (0, 10)
      chars <- vectorOf len genChar
      pure $ Some . StaticPiece . pack $ chars

genSomePath :: Gen (Some PathTemplate)
genSomePath = do
  len <- choose (0, 10)
  pieces <- vectorOf len genSomePiece
  pure $ buildSomePathTemplate pieces

buildSomePathTemplate :: (Foldable f) => f (Some PieceTemplate) -> Some PathTemplate
buildSomePathTemplate = foldr f (Some PathEnd)
  where
    f :: Some PieceTemplate -> Some PathTemplate -> Some PathTemplate
    f spiece spath =
      withSome spiece $ \piece ->
        withSome spath $ \path ->
          Some $ piece :/ path

invertComparison :: Ordering -> Ordering
invertComparison EQ = EQ
invertComparison LT = GT
invertComparison GT = LT
