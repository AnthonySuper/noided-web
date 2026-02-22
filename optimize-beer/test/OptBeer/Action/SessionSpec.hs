{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Action.SessionSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
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
import Test.Hspec

loginSpec :: TransactingSpec
loginSpec = describe "loginAction" $ do
  it "successfully logs in with correct credentials" $ \runner -> do
    now <- T.getCurrentTime
    -- 1. Setup: Create a user
    (_, user) <- runEff . runFailingError @SessionError . runFailingError @() . runWithRunner runner $ do
      runTransaction @() $ do
        actor <-
          querySingleRow $
            insertReturningAll
              Actor.actorsTable
              (values_ [#name :==> mutateVal_ (bindParam ("logintest" :: Text)) :::%? EmptyWrappedRow])
        user <-
          querySingleRow $
            insertReturningAll
              User.usersTable
              ( values_
                  [ #id
                      :==> mutateVal_ (bindParam actor.id)
                      :::%? #email
                      :==> mutateVal_ (bindParam ("login@example.com" :: Text))
                      :::%? EmptyWrappedRow
                  ]
              )
        let hw = UnsafeHashwordNothing "password123"
        _ <-
          querySingleRow $
            insertReturningAll
              UP.userPasswordsTable
              ( values_
                  [ #userId
                      :==> mutateVal_ (bindParam user.id)
                      :::%? #passwordDigest
                      :==> mutateVal_ (bindParam $ hashwordToPasswordHash hw)
                      :::%? EmptyWrappedRow
                  ]
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
      _ -> fail $ "Expected redirect to /, got something else"

    -- 3. Verify: Check session and login attempt
    runEff . runFailingError @SessionError . runFailingError @() . runWithRunner runner $ do
      (session, attempt) <- runTransaction @() $ do
        session <- querySingleRow $ do
          row <- addFrom_ (fromBase_ Session.sessionsTable)
          addWhere_ (row.userId ==. bindParam user.id)
          select_ row

        attempt <- querySingleRow $ do
          row <- addFrom_ (fromBase_ LA.loginAttemptsTable)
          addWhere_ (row.userId ==. bindParam user.id)
          select_ row
        return (session, attempt)
      liftIO $ do
        session.userId `shouldBe` user.id
        attempt.successful `shouldBe` True
        attempt.userId `shouldBe` user.id

spec :: TransactingSpec
spec = do
  loginSpec
