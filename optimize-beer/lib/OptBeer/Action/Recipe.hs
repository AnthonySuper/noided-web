{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Recipe where

import Control.Monad.Error.Class qualified as MonadError
import Data.Text (Text)
import Network.HTTP.Types.Status
import Noided.Form.HKD
import Noided.Sql
import OptBeer.Action.Base
import OptBeer.Action.Organization.Common (fetchMemberOrganization)
import OptBeer.Action.Search (useSearch)
import OptBeer.DB.Ids.OrganizationId (OrganizationId)
import OptBeer.DB.Table.Organization (OrganizationF (..))
import OptBeer.DB.Table.Recipe (RecipeF (..), recipesTable)
import OptBeer.DB.Type.Unit (Unit (..))
import OptBeer.Form.Type.Recipe (RecipeFormF (..))
import OptBeer.Form.Validate.Recipe (recipeValidator)
import OptBeer.Page.Recipe (recipeFormInternals, recipeFormPage, recipeFormWrapper, recipesIndexPage)
import OptBeer.Page.Type (Page)
import OptBeer.Routes (createRecipePath, newRecipePath, recipesPath, showOrganizationPath)
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))
import Optics.Core (view)

recipeActions ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error BadRequest :> es,
    Error SessionError :> es,
    GetRequestBody :> es,
    RunTransaction :> es,
    CurrentActor :> es,
    GetQueryParams :> es
  ) =>
  PageRoutes Page (Eff es)
recipeActions =
  actGet recipesPath recipesIndexAction
    <> actGet newRecipePath newRecipeAction
    <> actPost createRecipePath createRecipeAction

recipesIndexAction ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    GetQueryParams :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[OrganizationIdent] ->
  Eff es (PageResponse Page)
recipesIndexAction (ident :-$ RPNil) = do
  org <- fetchMemberOrganization ident
  searchForm <- parseForm . urlSubmissionToMultipartSubmission <$> getQueryParams
  recipes <- runInfallibleTransaction $ do
    queryVector $
      useSearch
        (\row -> toTSVector_ row.name `concatTSVector_` toTSVector_ row.description)
        ( do
            row <- addFrom_ (fromBase_ recipesTable)
            addWhere_ $ row.organizationId ==. bindParam org.id
            return row
        )
        searchForm
  return $ respondPage200 (recipesIndexPage org searchForm recipes)

newRecipeAction ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[OrganizationIdent] ->
  Eff es (PageResponse Page)
newRecipeAction (ident :-$ RPNil) = do
  org <- fetchMemberOrganization ident
  let defaultForm =
        RecipeForm
          { name = fieldInputFromTyped "",
            description = fieldInputFromTyped "",
            batchSize = fieldInputFromTyped 20,
            batchSizeUnit = fieldInputFromTyped Liter,
            targetOg = fieldInputFromTyped 1.050,
            targetFg = fieldInputFromTyped 1.010,
            targetAbv = fieldInputFromTyped 5.2,
            targetIbu = fieldInputFromTyped 35,
            targetSrm = fieldInputFromTyped 12,
            boilTimeMinutes = fieldInputFromTyped 60,
            targetEfficiency = fieldInputFromTyped 75
          }
  return $
    respondPage200
      ( recipeFormPage
          org
          ["organization.recipes.create.title"]
          ["organization.recipes.create.button"]
          (usePathTemplate createRecipePath ident)
          defaultForm
          mempty
      )

createRecipeAction ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error BadRequest :> es,
    Error SessionError :> es,
    GetRequestBody :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[OrganizationIdent] ->
  Eff es (PageResponse Page)
createRecipeAction (ident :-$ RPNil) = do
  org <- fetchMemberOrganization ident

  body <- hkdFormBody
  result <- runTransactionEither $ do
    -- 1. Validate form
    validated <- validateForm (recipeValidator org.id Nothing) body >>= either MonadError.throwError pure

    -- 2. Create recipe
    let recipeVals =
          singleValue_
            ( #organizationId :==> mutateVal_ (bindParam (org.id :: OrganizationId))
                :::%? #name :==> mutateVal_ (bindParam (view _InputResult validated.name :: Text))
                :::%? #description :==> mutateVal_ (bindParam (view _InputResult validated.description :: Text))
                :::%? #batchSize :==> mutateVal_ (bindParam validated.batchSize.val)
                :::%? #batchSizeUnit :==> mutateVal_ (bindParam validated.batchSizeUnit.val)
                :::%? #targetOg :==> mutateVal_ (bindParam validated.targetOg.val)
                :::%? #targetFg :==> mutateVal_ (bindParam validated.targetFg.val)
                :::%? #targetAbv :==> mutateVal_ (bindParam validated.targetAbv.val)
                :::%? #targetIbu :==> mutateVal_ (bindParam validated.targetIbu.val)
                :::%? #targetSrm :==> mutateVal_ (bindParam validated.targetSrm.val)
                :::%? #boilTimeMinutes :==> mutateVal_ (bindParam validated.boilTimeMinutes.val)
                :::%? #targetEfficiency :==> mutateVal_ (bindParam validated.targetEfficiency.val)
                :::%? EmptyWrappedRow
            )
    _ <- querySingleRow $ insertReturningAll recipesTable recipeVals
    return ()

  case result of
    Left err ->
      return $
        respondHKDForm
          (recipeFormWrapper org ["organization.recipes.create.title"])
          (recipeFormInternals ["organization.recipes.create.button"] (usePathTemplate createRecipePath ident) body)
          err
    Right () -> return $ RespondRedirect RedirectFound (usePathTemplate showOrganizationPath ident)
