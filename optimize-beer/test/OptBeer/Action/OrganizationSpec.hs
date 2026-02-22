{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module OptBeer.Action.OrganizationSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import Noided.Form
import Noided.Pathname
import Noided.Sql
import Noided.Web
import OptBeer.Action.Base
import OptBeer.Action.Organization
import OptBeer.DB.Table.Actor qualified as Actor
import OptBeer.DB.Table.User qualified as User
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.OrganizationUserAccess qualified as OUA
import OptBeer.DB.Table.UserDefaultOrganization qualified as UDO
import OptBeer.DB.Type.OrganizationAccessLevel
import OptBeer.Page.Type (Page)
import Test.Hspec
import OptBeer.Action.SpecHelper

createOrganizationSpec :: TransactingSpec
createOrganizationSpec = describe "createOrganizationAction" $ do
  it "successfully creates an organization for a logged-in user" $ \runner -> do
    -- 1. Setup: Create an actor and a user
    actor <- runEff . runFailingError @SessionError . runFailingError @() . runWithRunner runner $ do
      runTransaction @() $ do
        actor <- querySingleRow $
          insertReturningAll
            Actor.actorsTable
            (values_ [#name :==> mutateVal_ (bindParam ("orgcreator" :: Text)) :::%? EmptyWrappedRow])
        _ <- querySingleRow $
          insertReturningAll
            User.usersTable
            (values_ [#id :==> mutateVal_ (bindParam actor.id) :::%? #email :==> mutateVal_ (bindParam ("orgcreator@example.com" :: Text)) :::%? EmptyWrappedRow])
        return actor

    -- 2. Act: Create organization
    let formData =
          SubmissionObject $
            Map.fromList
              [ ("name", SubmissionValue (TextValue "My Organization"))
              ]
        body = FormBody (MultipartFormDataSubmission formData)

    resp <- runEff
      . runFailingError @SessionError
      . runFailingError @BadRequest
      . runFailingError @Unauthorized
      . runWithRequestBody body
      . runWithCurrentActor (Just actor)
      . runWithRunner runner
      $ do
        createOrganizationAction @_ @Page RPNil

    case resp of
      RespondRedirect RedirectFound "/" -> return ()
      _ -> fail "Expected redirect to /"

    -- 3. Verify: Check organization, access, and default
    (org, access, defaultOrg) <- runEff . runFailingError @SessionError . runFailingError @() . runWithRunner runner $ do
      runTransaction @() $ do
        org <- querySingleRow $ do
          row <- addFrom_ (fromBase_ Org.organizationsTable)
          addWhere_ (row.name ==. bindParam ("My Organization" :: Text))
          select_ row

        access <- querySingleRow $ do
          row <- addFrom_ (fromBase_ OUA.organizationUserAccessesTable)
          addWhere_ (row.organizationId ==. bindParam org.id)
          addWhere_ (row.userId ==. bindParam actor.id)
          select_ row

        defaultOrg <- querySingleRow $ do
          row <- addFrom_ (fromBase_ UDO.userDefaultOrganizationsTable)
          addWhere_ (row.userId ==. bindParam actor.id)
          select_ row

        return (org, access, defaultOrg)

    access.accessLevel `shouldBe` Admin
    defaultOrg.organizationId `shouldBe` org.id

  it "fails for unauthenticated users" $ \runner -> do
    let formData =
          SubmissionObject $
            Map.fromList
              [ ("name", SubmissionValue (TextValue "Unauthorized Org"))
              ]
        body = FormBody (MultipartFormDataSubmission formData)

    res <- runEff
      . runError @Unauthorized
      . runFailingError @SessionError
      . runFailingError @BadRequest
      . runWithRequestBody body
      . runWithCurrentActor Nothing
      . runWithRunner runner
      $ do
        createOrganizationAction @_ @Page RPNil

    case res of
      Left (_, Unauthorized _) -> return ()
      _ -> fail "Expected Unauthorized error"

  it "does not change default organization if one already exists" $ \runner -> do
    -- 1. Setup: Create actor, user, an existing org, and set it as default
    (actor, existingOrg) <- runEff . runFailingError @SessionError . runFailingError @() . runWithRunner runner $ do
      runTransaction @() $ do
        actor <- querySingleRow $
          insertReturningAll
            Actor.actorsTable
            (values_ [#name :==> mutateVal_ (bindParam ("multi-org-user" :: Text)) :::%? EmptyWrappedRow])
        _ <- querySingleRow $
          insertReturningAll
            User.usersTable
            (values_ [#id :==> mutateVal_ (bindParam actor.id) :::%? #email :==> mutateVal_ (bindParam ("multi@example.com" :: Text)) :::%? EmptyWrappedRow])
        existingOrg <- querySingleRow $
          insertReturningAll
            Org.organizationsTable
            (values_ [#name :==> mutateVal_ (bindParam ("Existing Org" :: Text)) :::%? EmptyWrappedRow])
        _ <- querySingleRow $
          insertReturningAll
            UDO.userDefaultOrganizationsTable
            (values_ [#userId :==> mutateVal_ (bindParam actor.id) :::%? #organizationId :==> mutateVal_ (bindParam existingOrg.id) :::%? EmptyWrappedRow])
        return (actor, existingOrg)

    -- 2. Act: Create another organization
    let formData =
          SubmissionObject $
            Map.fromList
              [ ("name", SubmissionValue (TextValue "New Org"))
              ]
        body = FormBody (MultipartFormDataSubmission formData)

    _ <- runEff
      . runFailingError @SessionError
      . runFailingError @BadRequest
      . runFailingError @Unauthorized
      . runWithRequestBody body
      . runWithCurrentActor (Just actor)
      . runWithRunner runner
      $ do
        createOrganizationAction @_ @Page RPNil

    -- 3. Verify: Default org should still be the existing one
    defaultOrg <- runEff . runFailingError @SessionError . runFailingError @() . runWithRunner runner $ do
      runTransaction @() $ do
        querySingleRow $ do
          row <- addFrom_ (fromBase_ UDO.userDefaultOrganizationsTable)
          addWhere_ (row.userId ==. bindParam actor.id)
          select_ row
    defaultOrg.organizationId `shouldBe` existingOrg.id

spec :: TransactingSpec
spec = do
  createOrganizationSpec
