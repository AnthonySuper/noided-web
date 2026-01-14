{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Form.Internal.FromFormSubmissionSpec (spec) where

import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics
import Noided.Form.Internal.FromFormSubmission
import Noided.Form.Internal.Type.FormContentType
import Noided.Form.Internal.Type.FormSubmission
import Test.Hspec

data TestUser = TestUser
  { name :: Text,
    age :: Int,
    email :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromFormSubmission ct TestUser

spec :: Spec
spec = do
  describe "FromFormSubmission (Generics)" $ do
    it "decodes a valid record" $ do
      let submission =
            SubmissionObject $
              Map.fromList
                [ ("name", SubmissionValue (TextValue "Alice")),
                  ("age", SubmissionValue (TextValue "30")),
                  ("email", SubmissionValue (TextValue "alice@example.com"))
                ]
      fromFormSubmission @'UrlEncoded submission `shouldBe` Right (TestUser "Alice" 30 (Just "alice@example.com"))

    it "decodes a record with missing optional field" $ do
      let submission =
            SubmissionObject $
              Map.fromList
                [ ("name", SubmissionValue (TextValue "Bob")),
                  ("age", SubmissionValue (TextValue "25"))
                ]
      fromFormSubmission @'UrlEncoded submission `shouldBe` Right (TestUser "Bob" 25 Nothing)

    it "fails when required field is missing" $ do
      let submission =
            SubmissionObject $
              Map.fromList
                [ ("name", SubmissionValue (TextValue "Charlie"))
                ]
      fromFormSubmission @'UrlEncoded @TestUser submission `shouldSatisfy` isLeft

    it "fails when field has wrong type" $ do
      let submission =
            SubmissionObject $
              Map.fromList
                [ ("name", SubmissionValue (TextValue "Dave")),
                  ("age", SubmissionValue (TextValue "not-a-number"))
                ]
      fromFormSubmission @'UrlEncoded @TestUser submission `shouldSatisfy` isLeft
