{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Action.SpecHelper.Setup where

import Data.Text (Text)
import Noided.Row
import Noided.Sql
import OptBeer.DB.Table.Actor (Actor)
import OptBeer.DB.Table.Actor qualified as Actor
import OptBeer.DB.Table.Organization (Organization)
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.OrganizationUserAccess qualified as OUA
import OptBeer.DB.Table.User qualified as User
import OptBeer.DB.Type.OrganizationAccessLevel (OrganizationAccessLevel (Member))

-- | Helper to create an actor, a corresponding user, and an organization where the user is a member.
createOrgWithMemberActor ::
  Text -> -- ^ Actor name
  Text -> -- ^ User email
  Text -> -- ^ Organization name
  TransactM () (Actor, Organization)
createOrgWithMemberActor actorName email orgName = do
  actor <-
    querySingleRow $
      insertReturningAll
        Actor.actorsTable
        (singleValue_ (#name :==> mutateVal_ (bindParam actorName) :::%? EmptyWrappedRow))
  _ <-
    querySingleRow $
      insertReturningAll
        User.usersTable
        (singleValue_ (#id :==> mutateVal_ (bindParam actor.id) :::%? #email :==> mutateVal_ (bindParam email) :::%? EmptyWrappedRow))
  org <-
    querySingleRow $
      insertReturningAll
        Org.organizationsTable
        (singleValue_ (#name :==> mutateVal_ (bindParam orgName) :::%? EmptyWrappedRow))
  _ <-
    querySingleRow $
      insertReturningAll
        OUA.organizationUserAccessesTable
        ( singleValue_
            ( #organizationId :==> mutateVal_ (bindParam org.id)
                :::%? #userId :==> mutateVal_ (bindParam actor.id)
                :::%? #accessLevel :==> mutateVal_ (bindParam Member)
                :::%? EmptyWrappedRow
            )
        )
  return (actor, org)
