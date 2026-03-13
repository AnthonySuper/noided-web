{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Item where

import Control.Monad.Error.Class qualified as MonadError
import Data.Text (Text)
import Lucid
import Noided.Form.HKD
import OptBeer.Action.Base
import OptBeer.Action.Organization.Common (fetchOrganization)
import OptBeer.DB.Ids.OrganizationId (OrganizationId)
import OptBeer.DB.Table.Actor (Actor)
import OptBeer.DB.Table.Item (itemsTable)
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Type.Unit (Unit)
import OptBeer.Form.Render.CreateItem (createItemRenderer)
import OptBeer.Form.Type.CreateItem
import OptBeer.Form.Validate.CreateItem (createItemValidator)
import OptBeer.Page.Type (Page)
import OptBeer.Routes (createItemPath, showOrganizationPath)
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
  actPost createItemPath createItemAction

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
  actor <- requireActor
  (org, _access) <- fetchOrganization actor ident

  body <- hkdFormBody
  result <- runTransactionEither $ do
    -- 1. Validate form
    validated <- validateForm (createItemValidator org.id) body >>= either MonadError.throwError pure

    -- 2. Create item
    let itemVals =
          singleValue_
            ( #organizationId
                :==> mutateVal_ (bindParam (org.id :: OrganizationId))
                :::%? #name
                :==> mutateVal_ (bindParam validated.name.val)
                :::%? #description
                :==> mutateVal_ (bindParam validated.description.val)
                :::%? #defaultUnit
                :==> mutateVal_ (bindParam validated.defaultUnit.val)
                :::%? EmptyWrappedRow
            )
    _ <- querySingleRow $ insertReturningAll itemsTable itemVals
    return ()

  case result of
    Left err ->
      return $
        RespondFormErrors
          (form_ [method_ "post", action_ (usePathTemplate createItemPath ident), class_ "form"])
          (renderFormT createItemRenderer body err)
    Right () -> return $ RespondRedirect RedirectFound (usePathTemplate showOrganizationPath ident)
