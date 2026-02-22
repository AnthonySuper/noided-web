{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Action.SessionSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (addUTCTime)
import Data.Time qualified as T
import Network.Socket (SockAddr (..))
import OptBeer.Action.Base
import OptBeer.Action.Session
import OptBeer.Action.SpecHelper
import OptBeer.DB.Table.Actor qualified as Actor
import OptBeer.DB.Table.LoginAttempt qualified as LA
import OptBeer.DB.Table.Session qualified as Session
import OptBeer.DB.Table.User qualified as User
import OptBeer.DB.Table.UserPassword qualified as UP
import OptBeer.Effect.HashPassword
import OptBeer.Page.Type (Page)
import OptBeer.Type.Hashword
import PostgreSQL.Binary.Range (Bound (..), Range (..))
import Test.Hspec

loginSpec :: TransactingSpec
loginSpec = describe "loginAction" $ do
  it "successfully logs in with correct credentials" $ \runner -> do
    now <- T.getCurrentTime
    -- 1. Setup: Create a user
    (_, user) <- runDBSetup runner $ do
        actor <-
          querySingleRow $
            insertReturningAll
              Actor.actorsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("logintest" :: Text)) :::%? EmptyWrappedRow))
        user <-
          querySingleRow $
            insertReturningAll
              User.usersTable
              ( singleValue_
                  ( #id
                      :==> mutateVal_ (bindParam actor.id)
                      :::%? #email
                      :==> mutateVal_ (bindParam ("login@example.com" :: Text))
                      :::%? EmptyWrappedRow
                  )
              )
        let hw = UnsafeHashwordNothing "password123"
        _ <-
          querySingleRow $
            insertReturningAll
              UP.userPasswordsTable
              ( singleValue_
                  ( #userId
                      :==> mutateVal_ (bindParam user.id)
                      :::%? #passwordDigest
                      :==> mutateVal_ (bindParam $ hashwordToPasswordHash hw)
                      :::%? EmptyWrappedRow
                  )
              )
        return (actor, user)

    -- 2. Act: Try to login
    let formData =
          SubmissionObject $
            Map.fromList
              [ ("email", SubmissionValue (TextValue "login@example.com")),
                ("password", SubmissionValue (TextValue "password123"))
              ]
        body = FormBody (MultipartFormDataSubmission formData)
        remoteIp = SockAddrInet 0 0 -- 0.0.0.0
    resp <- runEff
      . runFailingError @SessionError
      . runFailingError @BadRequest
      . runWithRequestBody body
      . runWithHeaders mempty
      . runWithRemoteIp remoteIp
      . runIgnoringCookies
      . runHashPasswordUnsafelyDoingNothing
      . runStaticTime now
      . runWithRunner runner
      $ do
        loginAction @_ @Page RPNil

    case resp of
      RespondRedirect RedirectFound "/" -> return ()
      _ -> fail "Expected redirect to /, got something else"

    -- 3. Verify: Check session and login attempt
    (session, attempt) <- runDBSetup runner $ do
        session <- querySingleRow $ do
          row <- addFrom_ (fromBase_ Session.sessionsTable)
          addWhere_ (row.userId ==. bindParam user.id)
          select_ row

        attempt <- querySingleRow $ do
          row <- addFrom_ (fromBase_ LA.loginAttemptsTable)
          addWhere_ (row.userId ==. bindParam user.id)
          select_ row
        return (session, attempt)

    session.userId `shouldBe` user.id
    attempt.successful `shouldBe` True
    attempt.userId `shouldBe` user.id

logoutSpec :: TransactingSpec
logoutSpec = describe "logoutAction" $ do
  it "successfully logs out and deletes the session" $ \runner -> do
    now <- T.getCurrentTime
    -- 1. Setup: Create a user and a session
    (user, session) <- runDBSetup runner $ do
        actor <-
          querySingleRow $
            insertReturningAll
              Actor.actorsTable
              (singleValue_ (#name :==> mutateVal_ (bindParam ("logouttest" :: Text)) :::%? EmptyWrappedRow))
        user <-
          querySingleRow $
            insertReturningAll
              User.usersTable
              ( singleValue_
                  ( #id
                      :==> mutateVal_ (bindParam actor.id)
                      :::%? #email
                      :==> mutateVal_ (bindParam ("logout@example.com" :: Text))
                      :::%? EmptyWrappedRow
                  )
              )
        let validUntil = addUTCTime sessionTtl now
            validDuring = Range (Incl now) (Excl validUntil)
        session <-
          querySingleRow $
            insertReturningAll
              Session.sessionsTable
              ( singleValue_
                  ( #userId
                      :==> mutateVal_ (bindParam user.id)
                      :::%? #userAgent
                      :==> mutateVal_ (bindParam (Nothing :: Maybe Text))
                      :::%? #remoteIp
                      :==> mutateVal_ (bindParam (sockAddrToIPRange (SockAddrInet 0 0)))
                      :::%? #validDuring
                      :==> mutateVal_ (bindParam validDuring)
                      :::%? EmptyWrappedRow
                  )
              )
        return (user, session)

    -- 2. Act: Logout
    resp <- runEff
      . runFailingError @SessionError
      . runWithRunner runner
      . runWithCurrentSessionId (Just session.id)
      . runStaticTime now
      . runIgnoringCookies
      $ do
        logoutAction RPNil

    case resp of
      RespondRedirect RedirectFound "/" -> return ()
      _ -> fail "Expected redirect to /, got something else"

    -- 3. Verify: Session should be gone
    mSession <- runDBSetup runner $ do
        queryMaybe $ do
          row <- addFrom_ (fromBase_ Session.sessionsTable)
          addWhere_ (row.id ==. bindParam session.id)
          select_ row
    mSession `shouldBe` Nothing

spec :: TransactingSpec
spec = do
  loginSpec
  logoutSpec
