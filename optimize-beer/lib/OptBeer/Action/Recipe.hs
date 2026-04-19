{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Recipe where

import Control.Monad.Error.Class qualified as MonadError
import Data.Foldable (fold, toList)
import Data.HKD
import Data.Int (Int32, Int64)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import GHC.Records
import Noided.Form.HKD
import Noided.Pathname
import Noided.Row
import Noided.Sql
import Noided.Web
import OptBeer.Action.Base
import OptBeer.Action.Organization.Common (fetchMemberOrganization)
import OptBeer.Action.Search (useSearch)
import OptBeer.DB.Ids.ItemId (ItemId (..), getItemId)
import OptBeer.DB.Ids.OrganizationId (OrganizationId)
import OptBeer.DB.Ids.RecipeId
import OptBeer.DB.Table.Organization
import OptBeer.DB.Table.Recipe
import OptBeer.DB.Table.RecipeIngredient
import OptBeer.DB.Type.Unit (Unit (..))
import OptBeer.Form.Type.Recipe
import OptBeer.Form.Validate.Recipe (recipeValidator)
import OptBeer.Page.Recipe (recipeFormInternals, recipeFormPage, recipeFormWrapper, recipesIndexPage)
import OptBeer.Page.Type (Page)
import OptBeer.Routes
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
    <> actGet editRecipePath editRecipeAction
    <> actPost updateRecipePath updateRecipeAction

editRecipeAction ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[RecipeId] ->
  Eff es (PageResponse Page)
editRecipeAction (recipeId :-$ RPNil) = do
  (org, recipe, ingredients) <- runInfallibleTransaction $ do
    recipe <- querySingleRow $ do
      row <- addFrom_ (fromBase_ recipesTable)
      addWhere_ (row.id ==. bindParam recipeId)
      return row
    org <- querySingleRow $ do
      row <- addFrom_ (fromBase_ organizationsTable)
      addWhere_ (row.id ==. bindParam recipe.organizationId)
      return row
    ingredients <- queryVector $ do
      row <- addFrom_ (fromBase_ recipeIngredientsTable)
      addWhere_ (row.recipeId ==. bindParam recipeId)
      return row
    return (org, recipe, ingredients)

  let formInput =
        RecipeForm
          { name = fieldInputFromTyped recipe.name,
            description = fieldInputFromTyped recipe.description,
            batchSize = fieldInputFromTyped recipe.batchSize,
            batchSizeUnit = fieldInputFromTyped recipe.batchSizeUnit,
            targetOg = fieldInputFromTyped recipe.targetOg,
            targetFg = fieldInputFromTyped recipe.targetFg,
            targetAbv = fieldInputFromTyped recipe.targetAbv,
            targetIbu = fieldInputFromTyped recipe.targetIbu,
            targetSrm = fieldInputFromTyped recipe.targetSrm,
            boilTimeMinutes = fieldInputFromTyped recipe.boilTimeMinutes,
            targetEfficiency = fieldInputFromTyped recipe.targetEfficiency,
            ingredients = ListInput $ Seq.fromList $ fmap (ingredientToInput) (toList ingredients)
          }
  return $
    respondPage200
      ( recipeFormPage
          org
          ["organization.recipes.edit.title"]
          ["organization.recipes.edit.button"]
          (usePathTemplate updateRecipePath recipeId)
          formInput
          mempty
      )

ingredientToInput :: RecipeIngredient -> FormInput (SubformField RecipeIngredientFormF)
ingredientToInput ri =
  SubformInput $
    RecipeIngredientForm
      { itemId = fieldInputFromTyped (getItemId ri.itemId),
        amount = fieldInputFromTyped ri.amount,
        amountUnit = fieldInputFromTyped ri.amountUnit,
        additionStage = fieldInputFromTyped ri.additionStage,
        additionTimeMinutes = fieldInputFromTyped (fromMaybe 0 ri.additionTimeMinutes),
        notes = fieldInputFromTyped ri.notes
      }

updateRecipeAction (((recipeId :: RecipeId) :-$ RPNil) :: RouteParams '[RecipeId]) = do
  (recipe, org) <- runInfallibleTransaction $ do
    recipe <- querySingleRow $ do
      row <- addFrom_ (fromBase_ recipesTable)
      addWhere_ (row.id ==. bindParam recipeId)
      return row
    org <- querySingleRow $ do
      row <- addFrom_ (fromBase_ organizationsTable)
      addWhere_ (row.id ==. bindParam recipe.organizationId)
      return row
    return (recipe, org)

  body <- hkdFormBody
  result <- runTransactionEither $ do
    -- 1. Validate form
    validated <- validateForm (recipeValidator org.id (Just recipeId)) body >>= either MonadError.throwError pure

    -- 2. Update recipe
    let recipeUpdates =
          #name |= mutateBound_ (view _InputResult validated.name :: Text)
            <> #description |= mutateBound_ (view _InputResult validated.description :: Text)
            <> #batchSize |= mutateBound_ validated.batchSize.val
            <> #batchSizeUnit |= mutateBound_ validated.batchSizeUnit.val
            <> #targetOg |= mutateBound_ validated.targetOg.val
            <> #targetFg |= mutateBound_ validated.targetFg.val
            <> #targetAbv |= mutateBound_ validated.targetAbv.val
            <> #targetIbu |= mutateBound_ validated.targetIbu.val
            <> #targetSrm |= mutateBound_ validated.targetSrm.val
            <> #boilTimeMinutes |= mutateBound_ validated.boilTimeMinutes.val
            <> #targetEfficiency |= mutateBound_ validated.targetEfficiency.val
            <> #updatedAt |= mutateVal_ now_
    _ <- querySingleRow $ updateReturningAll recipesTable $ \r -> do
      addWhere_ $ r.id ==. bindParam recipeId
      return recipeUpdates
    let ingredientVals = (validated.ingredients.val :: Seq.Seq (FormResult (SubformField RecipeIngredientFormF)))
    -- 3. Merge ingredients
    let ingredientRows =
          fmap
            ( \(SubformResult ri) ->
                (#recipeId :==> (bindParam @RecipeId recipeId))
                  :::%? #itemId :==> (bindParam (MkItemId ri.itemId.val))
                  :::%? #amount :==> (bindParam ri.amount.val)
                  :::%? #amountUnit :==> (bindParam ri.amountUnit.val)
                  :::%? #additionStage :==> (bindParam ri.additionStage.val)
                  :::%? #additionTimeMinutes :==> (bindParam (Just ri.additionTimeMinutes.val))
                  :::%? #notes :==> (bindParam ri.notes.val)
                  :::%? EmptyWrappedRow
            )
            ingredientVals

    case NE.nonEmpty (toList ingredientRows) of
      Nothing -> do
        -- If no ingredients, delete all for this recipe
        _ <- queryVector $ deleteReturning recipeIngredientsTable $ \row -> do
          addWhere_ (row.recipeId ==. bindParam recipeId)
          return $ Element $ bindParam @Int64 @NormalQuery 1
        return ()
      Just rows -> do
        let sourceValues = selectValues_ rows
        _ <-
          queryVector $
            mergeReturningAll
              recipeIngredientsTable
              sourceValues
              (\t s -> t.recipeId ==. s.recipeId &&. t.itemId ==. s.itemId)
              ( whenMatched_
                  ( MergeUpdate $ \_ s ->
                      #amount |= mutateVal_ s.amount
                        <> #amountUnit |= mutateVal_ s.amountUnit
                        <> #additionStage |= mutateVal_ s.additionStage
                        <> #additionTimeMinutes |= mutateVal_ s.additionTimeMinutes
                        <> #notes |= mutateVal_ s.notes
                  )
                  NE.:| [ whenNotMatched_ $ MergeInsert $ \_ s ->
                            singleValue_
                              ( #recipeId :==> mutateVal_ s.recipeId
                                  :::%? #itemId :==> mutateVal_ s.itemId
                                  :::%? #amount :==> mutateVal_ s.amount
                                  :::%? #amountUnit :==> mutateVal_ s.amountUnit
                                  :::%? #additionStage :==> mutateVal_ s.additionStage
                                  :::%? #additionTimeMinutes :==> mutateVal_ s.additionTimeMinutes
                                  :::%? #notes :==> mutateVal_ s.notes
                                  :::%? EmptyWrappedRow
                              ),
                          (andMergeCondition_ (\t _ -> t.recipeId ==. bindParam recipeId) (whenNotMatchedBySource_ MergeDelete))
                        ]
              )
        return ()
    return ()

  case result of
    Left err ->
      return $
        respondHKDForm
          (recipeFormWrapper org ["organization.recipes.edit.title"])
          (recipeFormInternals ["organization.recipes.edit.button"] (usePathTemplate updateRecipePath recipeId) body)
          err
    Right () -> return $ RespondRedirect RedirectFound (usePathTemplate showOrganizationPath (OrganizationById org.id))

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
  recipes <-
    runInfallibleTransaction $ do
      let searchQuery =
            useSearch
              (\row -> toTSVector_ row.name `concatTSVector_` toTSVector_ row.description)
              ( do
                  row <- addFrom_ (fromBase_ recipesTable)
                  addWhere_ (getField @"organizationId" row ==. bindParam org.id)
                  return row
              )
              searchForm
      queryVector searchQuery
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
            targetEfficiency = fieldInputFromTyped 75,
            ingredients = ListInput []
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
