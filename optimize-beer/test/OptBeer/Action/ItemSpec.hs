{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Action.ItemSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import Network.HTTP.Types (ok200)
import Noided.Form
import Noided.Pathname
import Noided.Sql
import Noided.Web
import OptBeer.Action.Base
import OptBeer.Action.Item
import OptBeer.Action.SpecHelper
import OptBeer.Action.SpecHelper.Setup (createActorWithUser, createOrgWithMemberActor)
import OptBeer.DB.Table.Item qualified as Item
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Type.Unit
import OptBeer.Routes (showOrganizationPath)
import OptBeer.Type.OrganizationIdent
import Test.Hspec

spec :: TransactingSpec
spec = do
  describe "itemsIndexAction" $ do
    it "successfully renders the items index for a user with access" $ \runner -> do
      (actor, org) <- runDBSetup runner $ createOrgWithMemberActor "IndexUser" "index@example.com" "Index Org"
      _ <- runDBSetup runner $ querySingleRow $ insertReturningAll Item.itemsTable (singleValue_ (#organizationId :==> mutateVal_ (bindParam org.id) :::%? #name :==> mutateVal_ (bindParam @Text "Item 1") :::%? #defaultUnit :==> mutateVal_ (bindParam Gram) :::%? EmptyWrappedRow))
      _ <- runDBSetup runner $ querySingleRow $ insertReturningAll Item.itemsTable (singleValue_ (#organizationId :==> mutateVal_ (bindParam org.id) :::%? #name :==> mutateVal_ (bindParam @Text "Item 2") :::%? #defaultUnit :==> mutateVal_ (bindParam Liter) :::%? EmptyWrappedRow))

      resp <- runEff
        . runFailingCommonErrors
        . runWithQueryParams SubmissionEmpty
        . runWithCurrentActor (Just actor)
        . runWithRunner runner
        $ do
          itemsIndexAction (OrganizationById org.id :-$ RPNil)

      case resp of
        RespondPage status _ -> status `shouldBe` ok200
        _ -> fail "Expected 200 OK"

  describe "showItemAction" $ do
    it "successfully renders an item for a user with access" $ \runner -> do
      (actor, org) <- runDBSetup runner $ createOrgWithMemberActor "ShowUser" "show@example.com" "Show Org"
      item <- runDBSetup runner $ querySingleRow $ insertReturningAll Item.itemsTable (singleValue_ (#organizationId :==> mutateVal_ (bindParam org.id) :::%? #name :==> mutateVal_ (bindParam @Text "Show Item") :::%? #defaultUnit :==> mutateVal_ (bindParam Gram) :::%? EmptyWrappedRow))

      resp <- runEff
        . runFailingCommonErrors
        . runWithCurrentActor (Just actor)
        . runWithRunner runner
        $ do
          showItemAction (item.id :-$ RPNil)

      case resp of
        RespondPage status _ -> status `shouldBe` ok200
        _ -> fail "Expected 200 OK"

  describe "createItemAction" $ do
    it "successfully creates an item for a user with access" $ \runner -> do
      -- 1. Setup: Create actor, user, organization, and access
      (actor, org) <- runDBSetup runner $ createOrgWithMemberActor "itemcreator" "itemcreator@example.com" "Item Org"

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
        . runFailingFormErrors
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
          actor <- createActorWithUser "noaccess" "noaccess@example.com"
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

  describe "editItemAction" $ do
    it "successfully renders the edit form for a user with access" $ \runner -> do
      (actor, org) <- runDBSetup runner $ createOrgWithMemberActor "Editor" "editor@example.com" "Edit Org"
      item <- runDBSetup runner $ querySingleRow $ insertReturningAll Item.itemsTable (singleValue_ (#organizationId :==> mutateVal_ (bindParam org.id) :::%? #name :==> mutateVal_ (bindParam @Text "Original Item") :::%? #defaultUnit :==> mutateVal_ (bindParam Gram) :::%? EmptyWrappedRow))

      resp <- runEff
        . runFailingCommonErrors
        . runWithCurrentActor (Just actor)
        . runWithRunner runner
        $ do
          editItemAction (item.id :-$ RPNil)

      case resp of
        RespondPage status _ -> status `shouldBe` ok200
        _ -> fail "Expected 200 OK with page content"

  describe "updateItemAction" $ do
    it "successfully updates an item for a user with access" $ \runner -> do
      (actor, org) <- runDBSetup runner $ createOrgWithMemberActor "Updater" "updater@example.com" "Update Org"
      item <- runDBSetup runner $ querySingleRow $ insertReturningAll Item.itemsTable (singleValue_ (#organizationId :==> mutateVal_ (bindParam org.id) :::%? #name :==> mutateVal_ (bindParam @Text "Old Item") :::%? #defaultUnit :==> mutateVal_ (bindParam Gram) :::%? EmptyWrappedRow))

      let formData =
            SubmissionObject $
              Map.fromList
                [ ("name", SubmissionValue (TextValue "Updated Item")),
                  ("description", SubmissionValue (TextValue "New description")),
                  ("defaultUnit", SubmissionValue (TextValue "liter"))
                ]
          body = FormBody (MultipartFormDataSubmission formData)

      resp <- runEff
        . runFailingFormErrors
        . runWithRequestBody body
        . runWithCurrentActor (Just actor)
        . runWithRunner runner
        $ do
          updateItemAction (item.id :-$ RPNil)

      case resp of
        RespondRedirect RedirectFound loc -> loc `shouldBe` usePathTemplate showOrganizationPath (OrganizationById org.id)
        _ -> fail "Expected redirect"

      updatedItem <- runDBSetup runner $ querySingleRow $ do
        row <- addFrom_ (fromBase_ Item.itemsTable)
        addWhere_ (row.id ==. bindParam item.id)
        select_ row
      updatedItem.name `shouldBe` "Updated Item"
      updatedItem.description `shouldBe` "New description"
      updatedItem.defaultUnit `shouldBe` Liter
