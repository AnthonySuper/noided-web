{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedRecordDot #-}

module OptBeer.Action.Organization.Common
  ( fetchOrganization,
    requireAccess,
    fetchMemberOrganization,
  )
where

import Control.Monad (when)
import OptBeer.Action.Base
import OptBeer.DB.Ids.ActorId (ActorId)
import OptBeer.DB.Ids.OrganizationId (OrganizationId)
import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.Organization
import OptBeer.DB.Table.OrganizationUserAccess
import OptBeer.DB.Type.OrganizationAccessLevel
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))

-- | Helper to fetch an organization and check user access.
-- Throws NotFound if the organization doesn't exist.
-- Throws Forbidden if the user doesn't have access.
fetchOrganization ::
  ( Error Forbidden :> es,
    Error NotFound :> es,
    Error SessionError :> es,
    RunTransaction :> es
  ) =>
  Actor ->
  OrganizationIdent ->
  Eff es (Organization, OrganizationUserAccess)
fetchOrganization actor ident = do
  mOrgAndAccess <- runInfallibleTransaction $ do
    orgMay <- queryMaybe $ do
      row <- addFrom_ (fromBase_ organizationsTable)
      case ident of
        OrganizationById rid -> addWhere_ (row.id ==. bindParam rid)
        OrganizationByName name -> addWhere_ (row.name ==. bindParam name)
      select_ row

    case orgMay of
      Nothing -> return Nothing
      Just org -> do
        accessMay <- queryMaybe $ do
          row <- addFrom_ (fromBase_ organizationUserAccessesTable)
          addWhere_ (row.organizationId ==. bindParam (org.id :: OrganizationId))
          addWhere_ (row.userId ==. bindParam (actor.id :: ActorId))
          select_ row
        return $ Just (org, accessMay)

  case mOrgAndAccess of
    Just (org, Just access) -> return (org, access)
    Just (_, Nothing) -> throwError $ Forbidden "You do not have access to this organization."
    Nothing -> throwError $ NotFound "Organization not found."

-- | Helper to ensure a user has at least a certain access level.
requireAccess :: (Error Forbidden :> es) => OrganizationAccessLevel -> OrganizationUserAccess -> Eff es ()
requireAccess required access =
  when (access.accessLevel < required) $
    throwError $ Forbidden "You do not have sufficient access to this organization."

-- | Helper to fetch an organization and ensure the current user is at least a member.
fetchMemberOrganization ::
  ( Error Unauthorized :> es,
    Error Forbidden :> es,
    Error NotFound :> es,
    Error SessionError :> es,
    RunTransaction :> es,
    CurrentActor :> es
  ) =>
  OrganizationIdent ->
  Eff es Organization
fetchMemberOrganization ident = do
  actor <- requireActor
  (org, access) <- fetchOrganization actor ident
  requireAccess Member access
  return org
