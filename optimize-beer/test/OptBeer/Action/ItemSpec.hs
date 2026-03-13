{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Action.ItemSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import Noided.Form
import Noided.Pathname
import Noided.Sql
import Noided.Web
import OptBeer.Action.Base
import OptBeer.Action.Item
import OptBeer.Action.SpecHelper
import OptBeer.DB.Table.Actor qualified as Actor
import OptBeer.DB.Table.Item qualified as Item
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.OrganizationUserAccess qualified as OUA
import OptBeer.DB.Table.User qualified as User
import OptBeer.DB.Type.OrganizationAccessLevel
import OptBeer.DB.Type.Unit
import OptBeer.Routes (showOrganizationPath)
import OptBeer.Type.OrganizationIdent
import Test.Hspec

spec :: TransactingSpec
spec = describe "createItemAction" $ do
  it "successfully creates an item for a user with access" $ \runner -> do
    -- 1. Setup: Create actor, user, organization, and access
    (actor, org) <- runDBSetup runner $ do
        actor <-
          querySingleRow $
            insertReturningAll
              Actor.actorsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("itemcreator" :: Text)) :::%? EmptyWrappedRow))
        _ <-
          querySingleRow $
            insertReturningAll
              User.usersTable
              (singleValue_ (#id :==> mutateVal_ (bindParam actor.id) :::%? #email :==> mutateVal_ (bindParam ("itemcreator@example.com" :: Text)) :::%? EmptyWrappedRow))
        org <-
          querySingleRow $
            insertReturningAll
              Org.organizationsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("Item Org" :: Text)) :::%? EmptyWrappedRow))
        _ <-
          querySingleRow $
            insertReturningAll
              OUA.organizationUserAccessesTable
              (singleValue_ (#organizationId :==> mutateVal_ (bindParam org.id) :::%? #userId :==> mutateVal_ (bindParam actor.id) :::%? #accessLevel :==> mutateVal_ (bindParam Admin) :::%? EmptyWrappedRow))
        return (actor, org)

    -- 2. Act: Create item
    let formData =
          SubmissionObject $
            Map.fromList
              [ ("name", SubmissionValue (TextValue "New Item")),
                ("description", SubmissionValue (TextValue "A great item")),
                ("defaultUnit", SubmissionValue (TextValue "gram"))
              ]
        body = FormBody (MultipartFormDataSubmission formData)

    resp <- runEff
      . runFailingError @SessionError
      . runFailingError @BadRequest
      . runFailingError @NotFound
      . runFailingError @Forbidden
      . runFailingError @Unauthorized
      . runWithRequestBody body
      . runWithCurrentActor (Just actor)
      . runWithRunner runner
      $ do
        createItemAction (OrganizationById org.id :-$ RPNil)

    -- 3. Verify: Check item exists and redirect
    item <- runDBSetup runner $ do
        querySingleRow $ do
          row <- addFrom_ (fromBase_ Item.itemsTable)
          addWhere_ (row.organizationId ==. bindParam org.id)
          addWhere_ (row.name ==. bindParam ("New Item" :: Text))
          select_ row

    case resp of
      RespondRedirect RedirectFound loc -> loc `shouldBe` usePathTemplate showOrganizationPath (OrganizationById org.id)
      _ -> fail "Expected redirect to organization show page"

    item.name `shouldBe` "New Item"
    item.description `shouldBe` "A great item"
    item.defaultUnit `shouldBe` Gram

  it "fails for a user without access" $ \runner -> do
    -- 1. Setup: Create actor, user, and organization (no access)
    (actor, org) <- runDBSetup runner $ do
        actor <-
          querySingleRow $
            insertReturningAll
              Actor.actorsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("noaccess" :: Text)) :::%? EmptyWrappedRow))
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

    -- 2. Act
    let formData =
          SubmissionObject $
            Map.fromList
              [ ("name", SubmissionValue (TextValue "Forbidden Item")),
                ("defaultUnit", SubmissionValue (TextValue "gram"))
              ]
        body = FormBody (MultipartFormDataSubmission formData)

    res <- runEff
      . runError @Forbidden
      . runFailingError @SessionError
      . runFailingError @BadRequest
      . runFailingError @NotFound
      . runFailingError @Unauthorized
      . runWithRequestBody body
      . runWithCurrentActor (Just actor)
      . runWithRunner runner
      $ do
        createItemAction (OrganizationById org.id :-$ RPNil)

    case res of
      Left (_, Forbidden _) -> return ()
      _ -> fail "Expected Forbidden error"
