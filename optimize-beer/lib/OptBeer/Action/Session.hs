{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Session
  ( sessionActions,
    logoutAction,
    loginAction,
    newSessionAction,
    sessionTtl,
    sockAddrToIPRange,
  )
where

import Control.Monad.Error.Class qualified as MonadError
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.IP
import Data.Map.Strict qualified as Map
import Data.Password.Bcrypt qualified as BC
import Data.Text (Text)
import Network.HTTP.Types.Status
import Data.Text.Encoding (decodeUtf8')
import Data.Time (addUTCTime)
import Data.Time qualified as T
import Lucid
import Network.HTTP.Types.Header (hUserAgent)
import Network.Socket (SockAddr (..))
import OptBeer.Action.Base
import OptBeer.DB.Ids.ActorId
import OptBeer.DB.Table.LoginAttempt
import OptBeer.DB.Table.Session
import OptBeer.DB.Table.User
import OptBeer.DB.Table.UserPassword
import OptBeer.Effect.HashPassword
import OptBeer.Form.Render.CreateSession
import OptBeer.Form.Type.CreateSession
import OptBeer.Form.Validate.CreateSession (createSessionValidator)
import OptBeer.Routes (logoutPath, newSessionPath, sessionsPath)
import OptBeer.Type.Hashword
import OptBeer.ValidationError.InvalidCredentials (InvalidCredentials (InvalidCredentials))
import Optics
import PostgreSQL.Binary.Range (Bound (..), Range (..))
import Web.Cookie qualified as Cookie

sessionActions ::
  ( FetchMessages renderM,
    FetchHtmlFormatters renderM,
    Error BadRequest :> es,
    Error SessionError :> es,
    GetRequestBody :> es,
    GetHeaders :> es,
    GetRemoteIp :> es,
    WriteCookie :> es,
    HashPassword :> es,
    RunTransaction :> es,
    CurrentTime :> es,
    CurrentSession :> es,
    IOE :> es
  ) =>
  PageRoutes renderM (Eff es)
sessionActions =
  actGet newSessionPath newSessionAction
    <> actPost sessionsPath loginAction
    <> actPost logoutPath logoutAction

wrapForm :: (Monad m) => HtmlT m a -> HtmlT m a
wrapForm act = form_ [method_ "post", action_ "/sessions", class_ "form"] $ do
  res <- act
  div_ [class_ "form-buttons"] $
    button_
      [class_ "button", type_ "submit"]
      "Login"
  return res

newSessionAction :: (FetchMessages renderM, FetchHtmlFormatters renderM, Monad m) => RouteParams '[] -> m (PageResponse renderM)
newSessionAction (RPNil :: RouteParams '[]) =
  return $
    respondPage200 $
      wrapForm $
        renderFormT createSessionRenderer hkdFormEmpty mempty

invalidCredentialsErrors :: FormErrors (SubformField CreateSessionF)
invalidCredentialsErrors =
  (mempty :: FormErrors (SubformField CreateSessionF))
    & #baseErrors
    .~ singletonError InvalidCredentials

collapseLefts :: Either a (Either a b) -> Either a b
collapseLefts = \case
  Left e -> Left e
  Right (Left e) -> Left e
  Right (Right a) -> Right a

loginAction ::
  ( Error BadRequest :> es,
    Error SessionError :> es,
    GetRequestBody :> es,
    GetHeaders :> es,
    GetRemoteIp :> es,
    WriteCookie :> es,
    HashPassword :> es,
    RunTransaction :> es,
    CurrentTime :> es,
    FetchMessages renderM,
    FetchHtmlFormatters renderM,
    IOE :> es
  ) =>
  RouteParams '[] ->
  Eff es (PageResponse renderM)
loginAction (RPNil :: RouteParams '[]) = do
  body <- hkdFormBody
  headers <- getHeaders
  remoteSock <- getRemoteIp
  let userAgent = Map.lookup hUserAgent headers >>= \v -> either (const Nothing) Just (decodeUtf8' v)
      remoteIp = sockAddrToIPRange remoteSock
  now <- getCurrentTime
  let validUntil = addUTCTime sessionTtl now
  result <- runTransactionEither $ do
    -- 1. Validate form (blank checks)
    validated <- validateForm createSessionValidator body >>= either MonadError.throwError pure

    -- 2. Try to find user and verify password
    userMay <- queryMaybe $ do
      user <- addFrom_ (fromBase_ usersTable)
      pw <- addFrom_ (fromBase_ userPasswordsTable)
      addWhere_ (user.id ==. pw.userId)
      addWhere_ (user.email ==. bindParam validated.email.val)
      return $ user :-: pw

    case userMay of
      Just (user :--: pw) -> do
        let digest = pw.passwordDigest
        case hashwordFromPasswordHash digest of
          Just hw ->
            case checkHashword validated.password.val hw of
              BC.PasswordCheckSuccess -> do
                logAttempt now user.id userAgent remoteIp True
                session <- createSession now user.id userAgent remoteIp
                return $ Right session
              BC.PasswordCheckFail -> do
                logAttempt now user.id userAgent remoteIp False
                return $ Left invalidCredentialsErrors
          Nothing -> return $ Left invalidCredentialsErrors
      Nothing -> return $ Left invalidCredentialsErrors

  case collapseLefts result of
    Left errs ->
      -- On validation failure
      return $
        respondHKDForm
          wrapForm
          (renderFormT createSessionRenderer body)
          errs
    Right session -> do
      setCookie $
        Cookie.defaultSetCookie
          { Cookie.setCookieName = "sessionId",
            Cookie.setCookieValue = LBS.toStrict (encode session.id),
            Cookie.setCookieHttpOnly = True,
            Cookie.setCookieSecure = True,
            Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
            Cookie.setCookieExpires = Just validUntil
          }
      return $ RespondRedirect RedirectFound "/"

logoutAction ::
  ( Error SessionError :> es,
    WriteCookie :> es,
    CurrentTime :> es,
    CurrentSession :> es,
    RunTransaction :> es,
    IOE :> es
  ) =>
  RouteParams '[] ->
  Eff es (PageResponse renderM)
logoutAction (RPNil :: RouteParams '[]) = do
  msid <- getCurrentSessionId

  case msid of
    Nothing -> return ()
    Just sid -> do
      _ <- runTransactionToResult $ do
        queryMaybe $ deleteReturning sessionsTable $ \row -> do
          addWhere_ (row.id ==. bindParam sid)
          return $ Element row.id
      return ()

  setCookie $
    Cookie.defaultSetCookie
      { Cookie.setCookieName = "sessionId",
        Cookie.setCookieValue = "",
        Cookie.setCookieExpires = Just (T.UTCTime (T.fromGregorian 1970 1 1) 0),
        Cookie.setCookieHttpOnly = True,
        Cookie.setCookieSecure = True,
        Cookie.setCookieSameSite = Just Cookie.sameSiteLax
      }
  return $ RespondRedirect RedirectFound "/"

logAttempt :: T.UTCTime -> ActorId -> Maybe Text -> IPRange -> Bool -> TransactM err ()
logAttempt (now :: T.UTCTime) (userId :: ActorId) (userAgent :: Maybe Text) (remoteIp :: IPRange) (successful :: Bool) = do
  let vals =
        singleValue_
          ( #userId
              :==> mutateVal_ (bindParam userId)
              :::%? #userAgent
              :==> mutateVal_ (bindParam userAgent)
              :::%? #remoteIp
              :==> mutateVal_ (bindParam remoteIp)
              :::%? #attemptAt
              :==> mutateVal_ (bindParam now)
              :::%? #successful
              :==> mutateVal_ (bindParam successful)
              :::%? EmptyWrappedRow
          )
  _ <- querySingleRow $ insertReturningAll loginAttemptsTable vals
  return ()

sessionTtl :: T.NominalDiffTime
sessionTtl = T.nominalDay

createSession :: T.UTCTime -> ActorId -> Maybe Text -> IPRange -> TransactM err Session
createSession now userId userAgent remoteIp = do
  -- Session valid for 24 hours
  let validUntil = addUTCTime sessionTtl now
      validDuring = Range (Incl now) (Excl validUntil)
      vals =
        singleValue_
          ( #userId
              :==> mutateVal_ (bindParam userId)
              :::%? #userAgent
              :==> mutateVal_ (bindParam userAgent)
              :::%? #remoteIp
              :==> mutateVal_ (bindParam remoteIp)
              :::%? #validDuring
              :==> mutateVal_ (bindParam validDuring)
              :::%? EmptyWrappedRow
          )
  querySingleRow $ insertReturningAll sessionsTable vals

sockAddrToIPRange :: SockAddr -> IPRange
sockAddrToIPRange s =
  case fromSockAddr s of
    Nothing ->
      -- Fallback for unexpected or non-IP socket addresses (e.g. Unix sockets).
      -- Use a sentinel IP range rather than crashing the request handler.
      let sentinelIPv4 = toIPv4 [0, 0, 0, 0]
       in IPv4Range $ makeAddrRange sentinelIPv4 0
    Just (ip, _) -> case ip of
      IPv4 v4addr -> IPv4Range $ makeAddrRange v4addr 32
      IPv6 v6addr -> IPv6Range $ makeAddrRange v6addr 128
