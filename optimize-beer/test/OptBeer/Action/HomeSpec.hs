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
import OptBeer.DB.Table.Actor qualified as Actor
import OptBeer.DB.Table.User qualified as User
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.UserDefaultOrganization qualified as UDO
import OptBeer.Page.Type (Page)
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
    actor <- runEff . runFailingError @SessionError . runFailingError @() . runWithRunner runner $ do
      runTransaction @() $ do
        querySingleRow $
          insertReturningAll
            Actor.actorsTable
            (values_ [#name :==> mutateVal_ (bindParam ("homeuser" :: Text)) :::%? EmptyWrappedRow])

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
    (actor, _) <- runEff . runFailingError @SessionError . runFailingError @() . runWithRunner runner $ do
      runTransaction @() $ do
        actor <- querySingleRow $
          insertReturningAll
            Actor.actorsTable
            (values_ [#name :==> mutateVal_ (bindParam ("homeuser2" :: Text)) :::%? EmptyWrappedRow])
        _ <- querySingleRow $
          insertReturningAll
            User.usersTable
            (values_ [#id :==> mutateVal_ (bindParam actor.id) :::%? #email :==> mutateVal_ (bindParam ("homeuser2@example.com" :: Text)) :::%? EmptyWrappedRow])
        org <- querySingleRow $
          insertReturningAll
            Org.organizationsTable
            (values_ [#name :==> mutateVal_ (bindParam ("Home Org" :: Text)) :::%? EmptyWrappedRow])
        _ <- querySingleRow $
          insertReturningAll
            UDO.userDefaultOrganizationsTable
            (values_ [#userId :==> mutateVal_ (bindParam actor.id) :::%? #organizationId :==> mutateVal_ (bindParam org.id) :::%? EmptyWrappedRow])
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
