module Noided.Web.Internal.Type.EndpointSpec (spec) where

import Data.Functor.Identity
import Data.List (sortOn)
import Network.HTTP.Types (StdMethod (..))
import Noided.Pathname (PathTemplate (..))
import Noided.Pathname.Internal.PieceTemplate (PieceTemplate (..))
import Noided.Web.Internal.Type.Endpoint
import Test.Hspec

-- Helper to create a simple endpoint for testing
mkTestEndpoint :: StdMethod -> PathTemplate pathParams -> SomeEndpoints Identity
mkTestEndpoint method pt = endpointOf method pt []

-- Helper to extract method/path pairs from endpoints
extractMethodPaths :: SomeEndpoints monad -> [(StdMethod, String)]
extractMethodPaths eps = sortOn snd $ map extract (getSomeEndpoints eps)
  where
    extract (SomeEndpoint method pt _) = (method, show pt)

spec :: Spec
spec = do
  describe "SomeEndpoints" $ do
    describe "Semigroup instance" $ do
      it "merges endpoints with the same path and method" $ do
        let path = StaticPiece "test" :/ PathEnd
        let ep1 = mkTestEndpoint GET path
        let ep2 = mkTestEndpoint GET path
        let merged = ep1 <> ep2
        length (getSomeEndpoints merged) `shouldBe` 1

      it "preserves endpoints with different paths" $ do
        let path1 = StaticPiece "test1" :/ PathEnd
        let path2 = StaticPiece "test2" :/ PathEnd
        let ep1 = mkTestEndpoint GET path1
        let ep2 = mkTestEndpoint GET path2
        let merged = ep1 <> ep2
        length (getSomeEndpoints merged) `shouldBe` 2

      it "preserves endpoints with different methods on same path" $ do
        let path = StaticPiece "test" :/ PathEnd
        let ep1 = mkTestEndpoint GET path
        let ep2 = mkTestEndpoint POST path
        let merged = ep1 <> ep2
        length (getSomeEndpoints merged) `shouldBe` 2

      it "handles multiple merges correctly" $ do
        let path1 = StaticPiece "test1" :/ PathEnd
        let path2 = StaticPiece "test2" :/ PathEnd
        let ep1 = mkTestEndpoint GET path1
        let ep2 = mkTestEndpoint POST path1
        let ep3 = mkTestEndpoint GET path2
        let ep4 = mkTestEndpoint GET path1 -- duplicate
        let merged = ep1 <> ep2 <> ep3 <> ep4
        -- Should have 3 endpoints: GET path1 (merged), POST path1, GET path2
        length (getSomeEndpoints merged) `shouldBe` 3

    describe "Monoid instance" $ do
      it "mempty produces empty endpoints" $ do
        length (getSomeEndpoints (mempty :: SomeEndpoints Identity)) `shouldBe` 0

      it "mempty is identity for (<>)" $ do
        let path = StaticPiece "test" :/ PathEnd
        let ep = mkTestEndpoint GET path
        extractMethodPaths (mempty <> ep) `shouldBe` extractMethodPaths ep
        extractMethodPaths (ep <> mempty) `shouldBe` extractMethodPaths ep

    describe "getSomeEndpoints" $ do
      it "returns empty list for empty endpoints" $ do
        length (getSomeEndpoints (mempty :: SomeEndpoints Identity)) `shouldBe` 0

      it "returns all endpoints in map" $ do
        let path1 = StaticPiece "test1" :/ PathEnd
        let path2 = StaticPiece "test2" :/ PathEnd
        let ep1 = mkTestEndpoint GET path1
        let ep2 = mkTestEndpoint POST path1
        let ep3 = mkTestEndpoint GET path2
        let merged = ep1 <> ep2 <> ep3
        length (getSomeEndpoints merged) `shouldBe` 3

    describe "cleanupSomeEndpoints" $ do
      it "is now a no-op (backwards compatibility)" $ do
        let path = StaticPiece "test" :/ PathEnd
        let ep = mkTestEndpoint GET path
        extractMethodPaths (cleanupSomeEndpoints ep) `shouldBe` extractMethodPaths ep

      it "endpoints are already merged" $ do
        let path = StaticPiece "test" :/ PathEnd
        let ep1 = mkTestEndpoint GET path
        let ep2 = mkTestEndpoint GET path
        let merged = ep1 <> ep2
        -- Should already be merged without needing cleanup
        length (getSomeEndpoints merged) `shouldBe` 1
        length (getSomeEndpoints (cleanupSomeEndpoints merged)) `shouldBe` 1
