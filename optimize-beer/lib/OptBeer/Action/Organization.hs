{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Organization where

import Control.Monad.Error.Class qualified as MonadError
import Lucid
import OptBeer.Action.Base
import OptBeer.DB.Ids.ActorId (ActorId)
import OptBeer.DB.Ids.OrganizationId (OrganizationId)
import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.Organization
import OptBeer.DB.Table.OrganizationUserAccess
import OptBeer.DB.Table.UserDefaultOrganization
import OptBeer.DB.Type.OrganizationAccessLevel
import OptBeer.Form.Render.CreateOrganization
import OptBeer.Form.Type.CreateOrganization
import OptBeer.Form.Validate.CreateOrganization (createOrganizationValidator)
import OptBeer.Page.Organization.Show (showOrganizationPage)
import OptBeer.Page.Type (Page)
import OptBeer.Routes (newOrganizationPath, organizationsPath, showOrganizationPath)
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))

organizationActions ::
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
organizationActions =
  actGet newOrganizationPath newOrganizationAction
    <> actPost organizationsPath createOrganizationAction
    <> actGet showOrganizationPath showOrganizationAction

wrapForm :: (Monad m) => HtmlT m a -> HtmlT m a
wrapForm act = form_ [method_ "post", action_ "/organizations", class_ "form"] $ do
  res <- act
  div_ [class_ "form-buttons"] $
    button_
      [class_ "button", type_ "submit"]
      "Create Organization"
  return res

newOrganizationAction ::
  ( Error Unauthorized :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[] ->
  Eff es (PageResponse Page)
newOrganizationAction (RPNil :: RouteParams '[]) = do
  mActor <- getCurrentActor
  case mActor of
    Nothing -> throwError $ Unauthorized "You must be logged in to create an organization."
    Just _ ->
      return $
        respondPage200 $
          wrapForm $
            renderFormT createOrganizationRenderer emptyCreateOrganizationForm mempty

createOrganizationAction ::
  ( Error Unauthorized :> es,
    Error BadRequest :> es,
    Error SessionError :> es,
    GetRequestBody :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[] ->
  Eff es (PageResponse Page)
createOrganizationAction (RPNil :: RouteParams '[]) = do
  mActor <- getCurrentActor
  case mActor of
    Nothing -> throwError $ Unauthorized "You must be logged in to create an organization."
    Just actor -> do
      body <- hkdFormBody
      result <- runTransactionEither $ do
        -- 1. Validate form
        validated <- validateForm createOrganizationValidator body >>= either MonadError.throwError pure

        -- 2. Create organization
        org <- querySingleRow $ insertReturningAll organizationsTable (singleValue_ (#name :==> mutateVal_ (bindParam validated.name.val) :::%? EmptyWrappedRow))

        -- 3. Add user as Admin
        let accessVals =
              singleValue_
                ( #organizationId :==> mutateVal_ (bindParam (org.id :: OrganizationId))
                    :::%? #userId :==> mutateVal_ (bindParam (actor.id :: ActorId))
                    :::%? #accessLevel :==> mutateVal_ (bindParam Admin)
                    :::%? EmptyWrappedRow
                )
        _ <- querySingleRow $ insertReturningAll organizationUserAccessesTable accessVals

        -- 4. Check if user has a default organization
        defaultOrgMay <- queryMaybe $ do
          row <- addFrom_ (fromBase_ userDefaultOrganizationsTable)
          addWhere_ (row.userId ==. bindParam (actor.id :: ActorId))
          return row

        case defaultOrgMay of
          Nothing -> do
            let defaultOrgVals =
                  singleValue_
                    ( #userId :==> mutateVal_ (bindParam (actor.id :: ActorId))
                        :::%? #organizationId :==> mutateVal_ (bindParam (org.id :: OrganizationId))
                        :::%? EmptyWrappedRow
                    )
            _ <- querySingleRow $ insertReturningAll userDefaultOrganizationsTable defaultOrgVals
            return ()
          Just _ -> return ()

        return (Right () :: Either (FormErrors (SubformField CreateOrganizationF)) ())

      case result of
        Left err ->
          return $
            RespondFormErrors
              wrapForm
              (renderFormT createOrganizationRenderer body err)
        Right _ -> return $ RespondRedirect RedirectFound "/"

showOrganizationAction ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[OrganizationIdent] ->
  Eff es (PageResponse Page)
showOrganizationAction (ident :-$ RPNil) = do
  mActor <- getCurrentActor
  actor <- case mActor of
    Nothing -> throwError $ Unauthorized "You must be logged in to view an organization."
    Just a -> return a

  mOrgAndAccess <- runTransactionToResult $ do
    orgMay <- queryMaybe $ do
      row <- addFrom_ (fromBase_ organizationsTable)
      case ident of
        OrganizationById rid -> addWhere_ (row.id ==. bindParam rid)
        OrganizationByName name -> addWhere_ (row.name ==. bindParam name)
      return row

    case orgMay of
      Nothing -> return Nothing
      Just org -> do
        accessMay <- queryMaybe $ do
          row <- addFrom_ (fromBase_ organizationUserAccessesTable)
          addWhere_ (row.organizationId ==. bindParam (org.id :: OrganizationId))
          addWhere_ (row.userId ==. bindParam (actor.id :: ActorId))
          return row
        return $ Just (org, accessMay)

  case mOrgAndAccess of
    TransactOK (Just (org, Just _)) -> return $ respondPage200 (showOrganizationPage org)
    TransactOK (Just (_, Nothing)) -> throwError $ Forbidden "You do not have access to this organization."
    TransactOK Nothing -> throwError $ NotFound "Organization not found."
    TransactErr _ -> throwError $ NotFound "Organization not found."
    SessionErr _ -> throwError $ NotFound "Organization not found."
