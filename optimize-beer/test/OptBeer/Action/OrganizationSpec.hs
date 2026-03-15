{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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
import OptBeer.Action.SpecHelper
import OptBeer.Action.SpecHelper.Setup (createOrgWithMemberActor)
import OptBeer.DB.Table.Actor qualified as Actor
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.OrganizationUserAccess qualified as OUA
import OptBeer.DB.Table.User qualified as User
import OptBeer.DB.Table.UserDefaultOrganization qualified as UDO
import OptBeer.DB.Type.OrganizationAccessLevel
import OptBeer.Routes (showOrganizationPath)
import OptBeer.Type.OrganizationIdent
import Test.Hspec

createOrganizationSpec :: TransactingSpec
createOrganizationSpec = describe "createOrganizationAction" $ do
  it "successfully creates an organization for a logged-in user" $ \runner -> do
    -- 1. Setup: Create an actor and a user
    actor <- runDBSetup runner $ do
        actor <-
          querySingleRow $
            insertReturningAll
              Actor.actorsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("orgcreator" :: Text)) :::%? EmptyWrappedRow))
        _ <-
          querySingleRow $
            insertReturningAll
              User.usersTable
              (singleValue_ (#id :==> mutateVal_ (bindParam actor.id) :::%? #email :==> mutateVal_ (bindParam ("orgcreator@example.com" :: Text)) :::%? EmptyWrappedRow))
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
        createOrganizationAction RPNil

    -- 3. Verify: Check organization, access, and default
    (org, access, defaultOrg) <- runDBSetup runner $ do
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

    case resp of
      RespondRedirect RedirectFound loc -> loc `shouldBe` usePathTemplate showOrganizationPath (OrganizationById org.id)
      _ -> fail "Expected redirect to organization show page"

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
        createOrganizationAction RPNil

    case res of
      Left (_, Unauthorized _) -> return ()
      _ -> fail "Expected Unauthorized error"

  it "does not change default organization if one already exists" $ \runner -> do
    -- 1. Setup: Create actor, user, an existing org, and set it as default
    (actor, existingOrg) <- runDBSetup runner $ do
        (actor, existingOrg) <- createOrgWithMemberActor "multi-org-user" "multi@example.com" "Existing Org"
        _ <-
          querySingleRow $
            insertReturningAll
              UDO.userDefaultOrganizationsTable
              (singleValue_ (#userId :==> mutateVal_ (bindParam actor.id) :::%? #organizationId :==> mutateVal_ (bindParam existingOrg.id) :::%? EmptyWrappedRow))
        return (actor, existingOrg)

    -- 2. Act: Create another organization
    let formData =
          SubmissionObject $
            Map.fromList
              [ ("name", SubmissionValue (TextValue "New Org"))
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
        createOrganizationAction RPNil

    -- 3. Verify: Default org should still be the existing one
    (newOrg, defaultOrg) <- runDBSetup runner $ do
        newOrg <- querySingleRow $ do
          row <- addFrom_ (fromBase_ Org.organizationsTable)
          addWhere_ (row.name ==. bindParam ("New Org" :: Text))
          select_ row
        defaultOrg <- querySingleRow $ do
          row <- addFrom_ (fromBase_ UDO.userDefaultOrganizationsTable)
          addWhere_ (row.userId ==. bindParam actor.id)
          select_ row
        return (newOrg, defaultOrg)

    case resp of
      RespondRedirect RedirectFound loc -> loc `shouldBe` usePathTemplate showOrganizationPath (OrganizationById newOrg.id)
      _ -> fail "Expected redirect to organization show page"

    defaultOrg.organizationId `shouldBe` existingOrg.id

  it "fails if organization name is only numbers" $ \runner -> do
    actor <- runDBSetup runner $ do
        actor <-
          querySingleRow $
            insertReturningAll
              Actor.actorsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("numberuser" :: Text)) :::%? EmptyWrappedRow))
        _ <-
          querySingleRow $
            insertReturningAll
              User.usersTable
              (singleValue_ (#id :==> mutateVal_ (bindParam actor.id) :::%? #email :==> mutateVal_ (bindParam ("numberuser@example.com" :: Text)) :::%? EmptyWrappedRow))
        return actor

    let formData =
          SubmissionObject $
            Map.fromList
              [ ("name", SubmissionValue (TextValue "12345"))
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
        createOrganizationAction RPNil

    case resp of
      RespondFormErrors {} -> return ()
      _ -> fail "Expected RespondFormErrors"

showOrganizationSpec :: TransactingSpec
showOrganizationSpec = describe "showOrganizationAction" $ do
  it "successfully shows an organization for a user with access" $ \runner -> do
    (actor, org) <- runDBSetup runner $ do
        actor <-
          querySingleRow $
            insertReturningAll
              Actor.actorsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("showuser" :: Text)) :::%? EmptyWrappedRow))
        _ <-
          querySingleRow $
            insertReturningAll
              User.usersTable
              (singleValue_ (#id :==> mutateVal_ (bindParam actor.id) :::%? #email :==> mutateVal_ (bindParam ("show@example.com" :: Text)) :::%? EmptyWrappedRow))
        org <-
          querySingleRow $
            insertReturningAll
              Org.organizationsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("Show Org" :: Text)) :::%? EmptyWrappedRow))
        _ <-
          querySingleRow $
            insertReturningAll
              OUA.organizationUserAccessesTable
              (singleValue_ (#organizationId :==> mutateVal_ (bindParam org.id) :::%? #userId :==> mutateVal_ (bindParam actor.id) :::%? #accessLevel :==> mutateVal_ (bindParam Admin) :::%? EmptyWrappedRow))
        return (actor, org)

    resp <- runEff
      . runFailingError @SessionError
      . runFailingError @Forbidden
      . runFailingError @NotFound
      . runFailingError @Unauthorized
      . runWithCurrentActor (Just actor)
      . runWithRunner runner
      $ do
        showOrganizationAction (OrganizationById org.id :-$ RPNil)

    case resp of
      RespondPage {} -> return ()
      _ -> fail "Expected RespondPage"

  it "fails with Forbidden for a user without access" $ \runner -> do
    (actor, org) <- runDBSetup runner $ do
        actor <-
          querySingleRow $
            insertReturningAll
              Actor.actorsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("noaccessuser" :: Text)) :::%? EmptyWrappedRow))
        _ <-
          querySingleRow $
            insertReturningAll
              User.usersTable
              (singleValue_ (#id :==> mutateVal_ (bindParam actor.id) :::%? #email :==> mutateVal_ (bindParam ("noaccess@example.com" :: Text)) :::%? EmptyWrappedRow))
        org <-
          querySingleRow $
            insertReturningAll
              Org.organizationsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("Forbidden Org" :: Text)) :::%? EmptyWrappedRow))
        return (actor, org)

    res <- runEff
      . runError @Forbidden
      . runFailingError @SessionError
      . runFailingError @NotFound
      . runFailingError @Unauthorized
      . runWithCurrentActor (Just actor)
      . runWithRunner runner
      $ do
        showOrganizationAction (OrganizationById org.id :-$ RPNil)

    case res of
      Left (_, Forbidden _) -> return ()
      _ -> fail "Expected Forbidden error"

  it "successfully shows an organization by name" $ \runner -> do
    (actor, _) <- runDBSetup runner $ do
        actor <-
          querySingleRow $
            insertReturningAll
              Actor.actorsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("nameuser" :: Text)) :::%? EmptyWrappedRow))
        _ <-
          querySingleRow $
            insertReturningAll
              User.usersTable
              (singleValue_ (#id :==> mutateVal_ (bindParam actor.id) :::%? #email :==> mutateVal_ (bindParam ("name@example.com" :: Text)) :::%? EmptyWrappedRow))
        org <-
          querySingleRow $
            insertReturningAll
              Org.organizationsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("NameOrg" :: Text)) :::%? EmptyWrappedRow))
        _ <-
          querySingleRow $
            insertReturningAll
              OUA.organizationUserAccessesTable
              (singleValue_ (#organizationId :==> mutateVal_ (bindParam org.id) :::%? #userId :==> mutateVal_ (bindParam actor.id) :::%? #accessLevel :==> mutateVal_ (bindParam Admin) :::%? EmptyWrappedRow))
        return (actor, org)

    resp <- runEff
      . runFailingError @SessionError
      . runFailingError @Forbidden
      . runFailingError @NotFound
      . runFailingError @Unauthorized
      . runWithCurrentActor (Just actor)
      . runWithRunner runner
      $ do
        showOrganizationAction (OrganizationByName "NameOrg" :-$ RPNil)

    case resp of
      RespondPage {} -> return ()
      _ -> fail "Expected RespondPage"

spec :: TransactingSpec
spec = do
  createOrganizationSpec
  showOrganizationSpec
