{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Action.RecipeSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import Effectful
import Network.HTTP.Types (ok200)
import Noided.Form
import Noided.Pathname
import Noided.Sql
import Noided.Web
import OptBeer.Action.Base
import OptBeer.Action.Recipe
import OptBeer.Action.SpecHelper
import OptBeer.Action.SpecHelper.Setup (createOrgWithMemberActor)
import OptBeer.DB.Table.Organization (OrganizationF (..))
import OptBeer.DB.Table.Recipe (RecipeF (..), recipesTable)
import OptBeer.DB.Type.Unit (Unit (..))
import OptBeer.Routes (showOrganizationPath)
import OptBeer.Type.OrganizationIdent
import Test.Hspec

spec :: TransactingSpec
spec = describe "Recipe actions" $ do
  describe "recipesIndexAction" $ do
    it "successfully renders the recipes index for a user with access" $ \runner -> do
      (actor, org) <- runDBSetup runner $ createOrgWithMemberActor "IndexUser" "index@example.com" "Index Org"
      _ <- runDBSetup runner $ querySingleRow $ insertReturningAll recipesTable (singleValue_ (#organizationId :==> mutateVal_ (bindParam org.id) :::%? #name :==> mutateVal_ (bindParam @Text "Recipe 1") :::%? #batchSize :==> mutateVal_ (bindParam (20 :: Scientific)) :::%? #batchSizeUnit :==> mutateVal_ (bindParam Liter) :::%? EmptyWrappedRow))
      
      resp <- runEff
        . runFailingCommonErrors
        . runWithQueryParams SubmissionEmpty
        . runWithCurrentActor (Just actor)
        . runWithRunner runner
        $ do
          recipesIndexAction (OrganizationById org.id :-$ RPNil)

      case resp of
        RespondPage status _ -> status `shouldBe` ok200
        _ -> fail "Expected 200 OK"

  describe "createRecipeAction" $ do
    it "successfully creates a recipe for a user with access" $ \runner -> do
      (actor, org) <- runDBSetup runner $ createOrgWithMemberActor "recipecreator" "recipecreator@example.com" "Recipe Org"

      let formData =
            SubmissionObject $
              Map.fromList
                [ ("name", SubmissionValue (TextValue "New Recipe")),
                  ("description", SubmissionValue (TextValue "A great beer")),
                  ("batchSize", SubmissionValue (TextValue "20")),
                  ("batchSizeUnit", SubmissionValue (TextValue "liter")),
                  ("targetOg", SubmissionValue (TextValue "1.050")),
                  ("targetFg", SubmissionValue (TextValue "1.010")),
                  ("targetAbv", SubmissionValue (TextValue "5.2")),
                  ("targetIbu", SubmissionValue (TextValue "35")),
                  ("targetSrm", SubmissionValue (TextValue "12")),
                  ("boilTimeMinutes", SubmissionValue (TextValue "60")),
                  ("targetEfficiency", SubmissionValue (TextValue "75"))
                ]
          body = FormBody (MultipartFormDataSubmission formData)

      resp <- runEff
        . runFailingFormErrors
        . runWithRequestBody body
        . runWithCurrentActor (Just actor)
        . runWithRunner runner
        $ do
          createRecipeAction (OrganizationById org.id :-$ RPNil)

      case resp of
        RespondRedirect RedirectFound loc -> loc `shouldBe` usePathTemplate showOrganizationPath (OrganizationById org.id)
        _ -> fail "Expected redirect to organization show page"

      -- Verify database state
      recipe <- runDBSetup runner $ do
        querySingleRow $ do
          row <- addFrom_ (fromBase_ recipesTable)
          addWhere_ $ row.organizationId ==. bindParam org.id
          addWhere_ $ row.name ==. bindParam ("New Recipe" :: Text)
          return row
      
      recipe.name `shouldBe` ("New Recipe" :: Text)
      recipe.batchSize `shouldBe` (20 :: Scientific)
      recipe.targetOg `shouldBe` (1.050 :: Scientific)
