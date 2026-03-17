{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Item where

import Control.Monad.Error.Class qualified as MonadError
import Data.Text (Text)
import Noided.Form.HKD
import Noided.Form.Types
import Noided.Sql
import OptBeer.Action.Base
import OptBeer.Action.Organization.Common (fetchMemberOrganization, requireAccess)
import OptBeer.Action.Search (useSearch)
import OptBeer.DB.Ids.ItemId (ItemId)
import OptBeer.DB.Ids.OrganizationId (OrganizationId)
import OptBeer.DB.Table.Actor (ActorF (id))
import OptBeer.DB.Table.Item (Item, ItemF (..), itemsTable)
import OptBeer.DB.Table.Organization (Organization, OrganizationF (..))
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.OrganizationUserAccess (OrganizationUserAccessF (..), organizationUserAccessesTable)
import OptBeer.DB.Type.OrganizationAccessLevel (OrganizationAccessLevel (..))
import OptBeer.DB.Type.Unit (Unit)
import OptBeer.Form.Type.Item (ItemFormF (..))
import OptBeer.Form.Validate.Item (itemValidator)
import OptBeer.Page.Item (itemFormInternals, itemFormPage, itemFormWrapper, itemsIndexPage, showItemPage)
import OptBeer.Page.Type (Page)
import OptBeer.Routes (createItemPath, editItemPath, itemsPath, newItemPath, showItemPath, showOrganizationPath, updateItemPath)
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))
import Optics.Core (view)

itemActions ::
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
itemActions =
  actGet itemsPath itemsIndexAction
    <> actGet newItemPath newItemAction
    <> actPost createItemPath createItemAction
    <> actGet showItemPath showItemAction
    <> actGet editItemPath editItemAction
    <> actPost updateItemPath updateItemAction

itemsIndexAction ::
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
itemsIndexAction (ident :-$ RPNil) = do
  org <- fetchMemberOrganization ident
  searchForm <- parseForm . urlSubmissionToMultipartSubmission <$> getQueryParams
  items <- runInfallibleTransaction $ do
    queryVector $
      useSearch
        (\row -> toTSVector_ row.name `concatTSVector_` toTSVector_ row.description)
        ( do
            row <- addFrom_ (fromBase_ itemsTable)
            addWhere_ $ row.organizationId ==. bindParam org.id
            return row
        )
        searchForm
  return $ respondPage200 (itemsIndexPage org searchForm items)

newItemAction ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[OrganizationIdent] ->
  Eff es (PageResponse Page)
newItemAction (ident :-$ RPNil) = do
  org <- fetchMemberOrganization ident
  return $
    respondPage200
      ( itemFormPage
          org
          ["organization.items.create.title"]
          ["organization.items.create.button"]
          (usePathTemplate createItemPath ident)
          hkdFormEmpty
          mempty
      )

createItemAction ::
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
createItemAction (ident :-$ RPNil) = do
  org <- fetchMemberOrganization ident

  body <- hkdFormBody
  result <- runTransactionEither $ do
    -- 1. Validate form
    validated <- validateForm (itemValidator org.id Nothing) body >>= either MonadError.throwError pure

    -- 2. Create item
    let itemVals =
          singleValue_
            ( #organizationId :==> mutateVal_ (bindParam (org.id :: OrganizationId))
                :::%? #name :==> mutateVal_ (bindParam (view _InputResult validated.name :: Text))
                :::%? #description :==> mutateVal_ (bindParam (view _InputResult validated.description :: Text))
                :::%? #defaultUnit :==> mutateVal_ (bindParam (view _InputResult validated.defaultUnit :: Unit))
                :::%? EmptyWrappedRow
            )
    _ <- querySingleRow $ insertReturningAll itemsTable itemVals
    return ()

  case result of
    Left err ->
      return $
        RespondFormErrors
          (itemFormWrapper org ["organization.items.create.title"])
          (itemFormInternals ["organization.items.create.button"] (usePathTemplate createItemPath ident) body err)
    Right () -> return $ RespondRedirect RedirectFound (usePathTemplate showOrganizationPath ident)

showItemAction ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[ItemId] ->
  Eff es (PageResponse Page)
showItemAction (itemId :-$ RPNil) = do
  (org, item) <- fetchMemberItem itemId
  return $ respondPage200 (showItemPage org item)

editItemAction ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[ItemId] ->
  Eff es (PageResponse Page)
editItemAction (itemId :-$ RPNil) = do
  (org, item) <- fetchMemberItem itemId
  let input =
        ItemForm
          { name = fieldInputFromTyped item.name,
            description = fieldInputFromTyped item.description,
            defaultUnit = fieldInputFromTyped item.defaultUnit
          }
  return $
    respondPage200
      ( itemFormPage
          org
          ["organization.items.edit.title"]
          ["organization.items.edit.button"]
          (usePathTemplate updateItemPath itemId)
          input
          mempty
      )

updateItemAction ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error BadRequest :> es,
    Error SessionError :> es,
    GetRequestBody :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[ItemId] ->
  Eff es (PageResponse Page)
updateItemAction (itemId :-$ RPNil) = do
  (org, _item) <- fetchMemberItem itemId

  body <- hkdFormBody
  result <- runTransactionEither $ do
    validated <- validateForm (itemValidator org.id (Just itemId)) body >>= either MonadError.throwError pure
    _ <- querySingleRow $ updateReturningAll itemsTable $ \r -> do
      addWhere_ $ r.id ==. bindParam itemId
      return $
        #name |= mutateVal_ (bindParam validated.name.val)
          <> #description |= mutateVal_ (bindParam validated.description.val)
          <> #defaultUnit |= mutateVal_ (bindParam validated.defaultUnit.val)
    return ()

  case result of
    Left err ->
      return $
        RespondFormErrors
          (itemFormWrapper org ["organization.items.edit.title"])
          (itemFormInternals ["organization.items.edit.button"] (usePathTemplate updateItemPath itemId) body err)
    Right () -> return $ RespondRedirect RedirectFound (usePathTemplate showOrganizationPath (OrganizationById org.id))

-- | Helper to fetch an item and ensure the current user has access to it.
fetchMemberItem ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  ItemId ->
  Eff es (Organization, Item)
fetchMemberItem itemId = do
  actor <- requireActor
  mOrgAndItem <- runInfallibleTransaction $ do
    queryMaybe $ do
      res@(item :-: _ :-: access) <-
        addFrom_ $
          fromBase_ itemsTable
            & innerJoin_ Org.organizationsTable
            `on_` (\item org -> item.organizationId ==. org.id)
            & innerJoin_ organizationUserAccessesTable
            `on_` (\(_ :-: org) access -> access.organizationId ==. org.id)
      addWhere_ $ item.id ==. bindParam itemId
      addWhere_ $ access.userId ==. bindParam actor.id
      return res
  case mOrgAndItem of
    Just (item :--: org :--: access) -> do
      requireAccess Member access
      return (org, item)
    Nothing -> throwError $ NotFound "Item not found."
