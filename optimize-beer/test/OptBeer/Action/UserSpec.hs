{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Action.UserSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Noided.Form
import Noided.Pathname
import Noided.Sql
import Noided.Web
import OptBeer.Action.SpecHelper
import OptBeer.Action.User
import OptBeer.DB.Table.Actor qualified as Actor
import OptBeer.DB.Table.User qualified as User
import OptBeer.Effect.HashPassword
import OptBeer.Page.Type (Page)
import Test.Hspec

createUserSpec :: TransactingSpec
createUserSpec = describe "createUserAction" $ do
  describe "with good parameters" $ do
    it "creates a new user and redirects" $ \runner -> do
      let formData =
            SubmissionObject $
              Map.fromList
                [ ("name", SubmissionValue (TextValue "testuser")),
                  ("email", SubmissionValue (TextValue "test@example.com")),
                  ("confirmEmail", SubmissionValue (TextValue "test@example.com")),
                  ("password", SubmissionValue (TextValue "Password123!")),
                  ("confirmPassword", SubmissionValue (TextValue "Password123!"))
                ]
          body = FormBody (MultipartFormDataSubmission formData)

      resp <- runEff . runFailingError @SessionError . runFailingError @BadRequest . runWithRequestBody body . runHashPasswordUnsafelyDoingNothing . runWithRunner runner $ do
        createUserAction @_ @Page RPNil

      case resp of
        RespondRedirect RedirectFound "/" -> return ()
        _ -> fail "Expected redirect to /"

      -- Verify database state
      (actor, user) <- runDBSetup runner $ do
        actor <- querySingleRow $ do
          row <- addFrom_ (fromBase_ Actor.actorsTable)
          addWhere_ (row.name ==. bindParam ("testuser" :: Text))
          select_ row
        user <- querySingleRow $ do
          row <- addFrom_ (fromBase_ User.usersTable)
          addWhere_ (row.email ==. bindParam ("test@example.com" :: Text))
          select_ row
        return (actor, user)

      actor.name `shouldBe` "testuser"
      user.email `shouldBe` "test@example.com"
      user.id `shouldBe` actor.id

  describe "with bad parameters" $ do
    it "returns form errors" $ \runner -> do
      let formData =
            SubmissionObject $
              Map.fromList
                [ ("name", SubmissionValue (TextValue "")), -- invalid name
                  ("email", SubmissionValue (TextValue "invalid-email")),
                  ("confirmEmail", SubmissionValue (TextValue "other-email")),
                  ("password", SubmissionValue (TextValue "short")),
                  ("confirmPassword", SubmissionValue (TextValue "mismatch"))
                ]
          body = FormBody (MultipartFormDataSubmission formData)

      resp <- runEff . runFailingError @SessionError . runFailingError @BadRequest . runWithRequestBody body . runHashPasswordUnsafelyDoingNothing . runWithRunner runner $ do
        createUserAction @_ @Page RPNil

      case resp of
        RespondFormErrors {} -> return ()
        _ -> fail "Expected RespondFormErrors"

      -- Verify database state: no user should have been created
      mUser <- runDBSetup runner $ do
        queryMaybe $ do
          row <- addFrom_ (fromBase_ User.usersTable)
          addWhere_ (row.email ==. bindParam ("invalid-email" :: Text))
          select_ row
      mUser `shouldBe` Nothing

spec :: TransactingSpec
spec = do
  createUserSpec
