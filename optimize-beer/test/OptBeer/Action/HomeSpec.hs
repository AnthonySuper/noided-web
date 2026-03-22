{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module OptBeer.Action.HomeSpec (spec) where

import Data.Text (Text)
import Effectful
import Noided.Pathname
import Noided.Sql
import Noided.Web
import OptBeer.Action.Base
import OptBeer.Action.Home
import OptBeer.Action.SpecHelper
import OptBeer.Action.SpecHelper.Setup (createOrgWithMemberActor)
import OptBeer.DB.Table.Actor qualified as Actor
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.UserDefaultOrganization qualified as UDO
import Test.Hspec

homeActionSpec :: TransactingSpec
homeActionSpec = describe "homeAction" $ do
  it "successfully renders for a guest" $ \runner -> do
    resp <- runEff
      . runFailingError @SessionError
      . runWithCurrentActor Nothing
      . runWithRunner runner
      $ do
        homeAction RPNil

    case resp of
      RespondPage {} -> return ()
      _ -> fail "Expected RespondPage"

  it "successfully renders for a logged-in user with no default org" $ \runner -> do
    actor <- runDBSetup runner $ do
        querySingleRow $
          insertReturningAll
            Actor.actorsTable
            (singleValue_ (#name :==> mutateVal_ (bindParam ("homeuser" :: Text)) :::%? EmptyWrappedRow))

    resp <- runEff
      . runFailingError @SessionError
      . runWithCurrentActor (Just actor)
      . runWithRunner runner
      $ do
        homeAction RPNil

    case resp of
      RespondPage {} -> return ()
      _ -> fail "Expected RespondPage"

  it "successfully renders for a logged-in user WITH a default org" $ \runner -> do
    (actor, _) <- runDBSetup runner $ do
        (actor, org) <- createOrgWithMemberActor "homeuser2" "homeuser2@example.com" "Home Org"
        _ <- querySingleRow $
          insertReturningAll
            UDO.userDefaultOrganizationsTable
            (singleValue_ (#userId :==> mutateVal_ (bindParam actor.id) :::%? #organizationId :==> mutateVal_ (bindParam org.id) :::%? EmptyWrappedRow))
        return (actor, org)

    resp <- runEff
      . runFailingError @SessionError
      . runWithCurrentActor (Just actor)
      . runWithRunner runner
      $ do
        homeAction RPNil

    case resp of
      RespondPage {} -> return ()
      _ -> fail "Expected RespondPage"

spec :: TransactingSpec
spec = do
  homeActionSpec
