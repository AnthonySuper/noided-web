{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Noided.Pathname.Internal.RouterSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Type.Equality
import Noided.Pathname.Internal.PathCaptures
import Noided.Pathname.Internal.PathTemplate
import Noided.Pathname.Internal.PieceTemplate
import Noided.Pathname.Internal.RouteParams
import Noided.Pathname.Internal.Router
import Test.Hspec
import Test.Inspection

data RoutedShowConst t captures where
  RoutedShowConst ::
    forall captures t.
    (Show (RouteParams captures), Eq (RouteParams captures), KnownPathCaptures captures) =>
    t ->
    RoutedShowConst t captures

deriving instance (Show t) => Show (RoutedShowConst t captures)

deriving instance (Eq t) => Eq (RoutedShowConst t captures)

data RoutedResult t where
  RouteResult ::
    RouteParams captures ->
    RoutedShowConst t captures ->
    RoutedResult t

instance (Show t) => Show (RoutedResult t) where
  showsPrec d (RouteResult rp rcs@(RoutedShowConst _)) =
    showParen (d > 10) $
      showString "RouteResult "
        . showsPrec 11 rp
        . showString " "
        . showsPrec 11 rcs

instance (Eq t) => Eq (RoutedResult t) where
  (RouteResult rp rcs@(RoutedShowConst @captures _)) == (RouteResult rp' rcs'@(RoutedShowConst @captures' _)) =
    case testEquality (pathCaptureSing @captures) (pathCaptureSing @captures') of
      Nothing -> False
      Just Refl -> rp == rp' && rcs == rcs'

routedResult :: RouteMatch (RoutedShowConst t) -> RoutedResult t
routedResult (RouteMatched caps t) = RouteResult caps t

singleRouterSpec :: Spec
singleRouterSpec = describe "a singleton router" $ do
  let routedVal = RoutedShowConst (1 :: Int)
  let router = singletonRouter (StaticPiece "users" :/ capPiece @Int :/ PathEnd) routedVal
  it "returns nothing if given empty path" $
    fmap routedResult (firstRouterMatch [] router) `shouldBe` Nothing
  it "returns a correct value if present" $
    fmap routedResult (firstRouterMatch ["users", "1"] router) `shouldBe` Just (RouteResult (1 :-$ RPNil) routedVal)
  it "returns a correct value if present with trailing slash" $
    fmap routedResult (firstRouterMatch ["users", "5", ""] router) `shouldBe` Just (RouteResult (5 :-$ RPNil) routedVal)

multipleStaticRoutesSpec :: Spec
multipleStaticRoutesSpec = describe "multiple static routes" $ do
  let aboutVal = RoutedShowConst ("about" :: String)
  let contactVal = RoutedShowConst ("contact" :: String)
  let homeVal = RoutedShowConst ("home" :: String)
  let router =
        singletonRouter (StaticPiece "about" :/ PathEnd) aboutVal
          <> singletonRouter (StaticPiece "contact" :/ PathEnd) contactVal
          <> singletonRouter PathEnd homeVal
  it "matches home route" $
    fmap routedResult (firstRouterMatch [] router) `shouldBe` Just (RouteResult RPNil homeVal)
  it "matches about route" $
    fmap routedResult (firstRouterMatch ["about"] router) `shouldBe` Just (RouteResult RPNil aboutVal)
  it "matches contact route" $
    fmap routedResult (firstRouterMatch ["contact"] router) `shouldBe` Just (RouteResult RPNil contactVal)
  it "returns nothing for unknown route" $
    fmap routedResult (firstRouterMatch ["unknown"] router) `shouldBe` Nothing

multipleDynamicRoutesSpec :: Spec
multipleDynamicRoutesSpec = describe "multiple dynamic routes" $ do
  let userVal = RoutedShowConst ("user" :: String)
  let postVal = RoutedShowConst ("post" :: String)
  let router =
        singletonRouter (StaticPiece "users" :/ capPiece @Int :/ PathEnd) userVal
          <> singletonRouter (StaticPiece "posts" :/ capPiece @Int :/ PathEnd) postVal
  it "matches user route" $
    fmap routedResult (firstRouterMatch ["users", "42"] router) `shouldBe` Just (RouteResult (42 :-$ RPNil) userVal)
  it "matches post route" $
    fmap routedResult (firstRouterMatch ["posts", "100"] router) `shouldBe` Just (RouteResult (100 :-$ RPNil) postVal)
  it "returns nothing for invalid capture" $
    fmap routedResult (firstRouterMatch ["users", "invalid"] router) `shouldBe` Nothing

mixedRoutesSpec :: Spec
mixedRoutesSpec = describe "mixed static and dynamic routes" $ do
  let usersIndexVal = RoutedShowConst ("users_index" :: String)
  let userShowVal = RoutedShowConst ("user_show" :: String)
  let userEditVal = RoutedShowConst ("user_edit" :: String)
  let router =
        singletonRouter (StaticPiece "users" :/ PathEnd) usersIndexVal
          <> singletonRouter (StaticPiece "users" :/ capPiece @Int :/ PathEnd) userShowVal
          <> singletonRouter (StaticPiece "users" :/ capPiece @Int :/ StaticPiece "edit" :/ PathEnd) userEditVal
  it "matches users index" $
    fmap routedResult (firstRouterMatch ["users"] router) `shouldBe` Just (RouteResult RPNil usersIndexVal)
  it "matches user show" $
    fmap routedResult (firstRouterMatch ["users", "42"] router) `shouldBe` Just (RouteResult (42 :-$ RPNil) userShowVal)
  it "matches user edit" $
    fmap routedResult (firstRouterMatch ["users", "42", "edit"] router) `shouldBe` Just (RouteResult (42 :-$ RPNil) userEditVal)

crudRoutesSpec :: Spec
crudRoutesSpec = describe "CRUD-style routes" $ do
  let indexVal = RoutedShowConst ("index" :: String)
  let newVal = RoutedShowConst ("new" :: String)
  let showVal = RoutedShowConst ("show" :: String)
  let editVal = RoutedShowConst ("edit" :: String)
  let router =
        singletonRouter (StaticPiece "resources" :/ PathEnd) indexVal
          <> singletonRouter (StaticPiece "resources" :/ StaticPiece "new" :/ PathEnd) newVal
          <> singletonRouter (StaticPiece "resources" :/ capPiece @Int :/ PathEnd) showVal
          <> singletonRouter (StaticPiece "resources" :/ capPiece @Int :/ StaticPiece "edit" :/ PathEnd) editVal
  it "matches index" $
    fmap routedResult (firstRouterMatch ["resources"] router) `shouldBe` Just (RouteResult RPNil indexVal)
  it "matches new" $
    fmap routedResult (firstRouterMatch ["resources", "new"] router) `shouldBe` Just (RouteResult RPNil newVal)
  it "matches show" $
    fmap routedResult (firstRouterMatch ["resources", "123"] router) `shouldBe` Just (RouteResult (123 :-$ RPNil) showVal)
  it "matches edit" $
    fmap routedResult (firstRouterMatch ["resources", "123", "edit"] router) `shouldBe` Just (RouteResult (123 :-$ RPNil) editVal)
  it "prefers static 'new' over dynamic capture" $
    fmap routedResult (firstRouterMatch ["resources", "new"] router) `shouldBe` Just (RouteResult RPNil newVal)

multiCaptureSpec :: Spec
multiCaptureSpec = describe "multiple captures in a route" $ do
  let pathTemplate = StaticPiece "users" :/ capPiece @Int :/ StaticPiece "posts" :/ capPiece @Int :/ PathEnd
      nestedVal = RoutedShowConst ("nested" :: String)
      router = singletonRouter pathTemplate nestedVal
  it "matches with two captures" $
    isJust (firstRouterMatch ["users", "5", "posts", "42"] router) `shouldBe` True
  it "returns nothing for partial match" $
    isNothing (firstRouterMatch ["users", "5", "posts"] router) `shouldBe` True

edgeCasesSpec :: Spec
edgeCasesSpec = describe "edge cases" $ do
  let rootVal = RoutedShowConst ("root" :: String)
  let router = singletonRouter PathEnd rootVal
  it "matches empty path" $
    fmap routedResult (firstRouterMatch [] router) `shouldBe` Just (RouteResult RPNil rootVal)
  it "matches path with single empty string" $
    fmap routedResult (firstRouterMatch [""] router) `shouldBe` Just (RouteResult RPNil rootVal)
  it "returns nothing for non-empty path" $
    fmap routedResult (firstRouterMatch ["something"] router) `shouldBe` Nothing

expectPassInspection :: Result -> Expectation
expectPassInspection a = a `shouldSatisfy` passedInspection
  where
    passedInspection (Success _) = True
    passedInspection _ = False

inspectionSpec :: Spec
inspectionSpec = describe "inspection tests" $ do
  it "does not use runtime proofs" $
    expectPassInspection inspectFoldrCaptureMapErasesProof
  return ()

spec :: Spec
spec = do
  singleRouterSpec
  multipleStaticRoutesSpec
  multipleDynamicRoutesSpec
  mixedRoutesSpec
  crudRoutesSpec
  multiCaptureSpec
  edgeCasesSpec
  inspectionSpec
  return ()
