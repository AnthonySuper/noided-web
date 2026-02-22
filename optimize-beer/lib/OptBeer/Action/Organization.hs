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
import OptBeer.Routes (newOrganizationPath, organizationsPath)

organizationActions ::
  ( FetchMessages renderM,
    FetchHtmlFormatters renderM,
    Error Unauthorized :> es,
    Error BadRequest :> es,
    Error SessionError :> es,
    GetRequestBody :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  PageRoutes renderM (Eff es)
organizationActions =
  actGet newOrganizationPath newOrganizationAction
    <> actPost organizationsPath createOrganizationAction

wrapForm :: (Monad m) => HtmlT m a -> HtmlT m a
wrapForm act = form_ [method_ "post", action_ "/organizations", class_ "form"] $ do
  res <- act
  div_ [class_ "form-buttons"] $
    button_
      [class_ "button", type_ "submit"]
      "Create Organization"
  return res

newOrganizationAction ::
  ( FetchMessages renderM,
    FetchHtmlFormatters renderM,
    Error Unauthorized :> es,
    CurrentActor :> es
  ) =>
  RouteParams '[] ->
  Eff es (PageResponse renderM)
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
    CurrentActor :> es,
    FetchMessages renderM,
    FetchHtmlFormatters renderM
  ) =>
  RouteParams '[] ->
  Eff es (PageResponse renderM)
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
