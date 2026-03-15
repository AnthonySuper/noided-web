{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Item where

import Control.Monad.Error.Class qualified as MonadError
import Data.Text (Text)
import Lucid
import Noided.Form.HKD
import OptBeer.Action.Base
import OptBeer.Action.Organization.Common (fetchMemberOrganization)
import OptBeer.DB.Ids.OrganizationId (OrganizationId)
import OptBeer.DB.Table.Item (itemsTable)
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Type.Unit (Unit)
import OptBeer.Form.Render.CreateItem (createItemRenderer)
import OptBeer.Form.Type.CreateItem (CreateItemF (..))
import OptBeer.Form.Validate.CreateItem (createItemValidator)
import OptBeer.Page.Item.New (newItemPage)
import OptBeer.Page.Type (Page)
import OptBeer.Routes (createItemPath, newItemPath, showOrganizationPath)
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
    CurrentActor :> es
  ) =>
  PageRoutes Page (Eff es)
itemActions =
  actGet newItemPath newItemAction
    <> actPost createItemPath createItemAction

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
  return $ respondPage200 (newItemPage org hkdFormEmpty mempty)

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
    validated <- validateForm (createItemValidator org.id) body >>= either MonadError.throwError pure

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
          (form_ [method_ "post", action_ (usePathTemplate createItemPath ident), class_ "form", data_ "framelike" "true"])
          (renderFormT createItemRenderer body err)
    Right () -> return $ RespondRedirect RedirectFound (usePathTemplate showOrganizationPath ident)
