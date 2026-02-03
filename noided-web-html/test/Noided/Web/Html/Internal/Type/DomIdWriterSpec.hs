{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Monoid law, right identity" -}
{- HLINT ignore "Monoid law, left identity" -}

module Noided.Web.Html.Internal.Type.DomIdWriterSpec (spec) where

import Noided.Web.Html.Internal.Type.DomIdWriter
import Test.Hspec

spec :: Spec
spec = describe "DomIdWriter" $ do
  it "monoid mempty is empty text" $
    domIdToText mempty `shouldBe` ""

  it "semigroup <> joins with --" $
    domIdToText ("foo" <> "bar") `shouldBe` "foo--bar"

  it "semigroup <> joins multiple with --" $
    domIdToText ("foo" <> "bar" <> "baz") `shouldBe` "foo--bar--baz"

  it "unwritten <> written is written" $
    domIdToText (mempty <> "foo") `shouldBe` "foo"

  it "written <> unwritten is written" $
    domIdToText ("foo" <> mempty) `shouldBe` "foo"

  it "domIdPiece creates a piece" $
    domIdToText (domIdPiece "foo") `shouldBe` "foo"

  it "domIdPieceString creates a piece" $
    domIdToText (domIdPieceString "foo") `shouldBe` "foo"
