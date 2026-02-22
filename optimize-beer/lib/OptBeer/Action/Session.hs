{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Session where

import Control.Monad.Error.Class qualified as MonadError
import Data.ByteString.Char8 qualified as B8
import Data.IP
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Password.Bcrypt qualified as BC
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (addUTCTime)
import Data.Time qualified as T
import Effectful
import Effectful.Error.Static
import Lucid
import Network.HTTP.Types.Header (hUserAgent)
import Network.Socket (SockAddr (..))
import Noided.Form
import Noided.Form.HKD
import Noided.Pathname
import Noided.Row
import Noided.Sql
import Noided.Web
import Noided.Web.Html.FormRender
import OptBeer.DB.Ids.ActorId
import OptBeer.DB.Table.LoginAttempt
import OptBeer.DB.Table.Session
import OptBeer.DB.Table.User
import OptBeer.DB.Table.UserPassword
import OptBeer.Effect.HashPassword
import OptBeer.Error.BadRequest
import OptBeer.Form.Render.CreateSession
import OptBeer.Form.Type.CreateSession
import OptBeer.Form.Validate.CreateSession (createSessionValidator)
import OptBeer.Routes (newSessionPath, sessionsPath)
import OptBeer.Type.Hashword
import Optics
import PostgreSQL.Binary.Range (Bound (..), Range (..))
import Web.Cookie qualified as Cookie
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS

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
    IOE :> es
  ) =>
  PageRoutes renderM (Eff es)
sessionActions =
  actGet newSessionPath newSessionAction
    <> actPost sessionsPath loginAction

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
        renderFormT createSessionRenderer emptyCreateSessionForm mempty

hkdFormBody ::
  ( Error BadRequest :> es,
    GetRequestBody :> es,
    HKDForm t
  ) =>
  Eff es (t FormInput)
hkdFormBody = do
  reqBody <- getRequestBody
  case reqBody of
    NoBody -> throwError $ BadRequest "no body"
    FormBody sfb -> return $ parseForm $ multipartFromSomeSubmission sfb
    JSONBody _ -> throwError $ BadRequest "json body unexpected"
    MalformedBody v -> throwError $ BadRequest v
    UnknownBody _ -> throwError $ BadRequest "unknown body type"

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
                return $ Left ()
          Nothing -> return $ Left ()
      Nothing -> return $ Left ()

  case result of
    Left _ ->
      -- On validation failure
      return $
        RespondFormErrors
          wrapForm
          (renderFormT createSessionRenderer body mempty)
    Right (Left ()) ->
      -- On credential failure
      return $
        RespondFormErrors
          wrapForm
          (renderFormT createSessionRenderer body mempty) -- TODO: Add generic error
    Right (Right session) -> do
      setCookie $
        Cookie.defaultSetCookie
          { Cookie.setCookieName = "sessionId",
            Cookie.setCookieValue = LBS.toStrict (encode session.id),
            Cookie.setCookieHttpOnly = True,
            Cookie.setCookieSecure = True,
            Cookie.setCookieSameSite = Just Cookie.sameSiteLax
          }
      return $ RespondRedirect RedirectFound "/"

logAttempt now userId userAgent remoteIp successful = do
  let vals =
        values_
          [ #userId
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
          ]
  _ <- querySingleRow $ insertReturningAll loginAttemptsTable vals
  return ()

createSession now userId userAgent remoteIp = do
  -- Session valid for 24 hours
  let validUntil = addUTCTime (24 * 60 * 60) now
      validDuring = Range (Incl now) (Excl validUntil)
      vals =
        values_
          [ #userId
              :==> mutateVal_ (bindParam userId)
              :::%? #userAgent
              :==> mutateVal_ (bindParam userAgent)
              :::%? #remoteIp
              :==> mutateVal_ (bindParam remoteIp)
              :::%? #validDuring
              :==> mutateVal_ (bindParam validDuring)
              :::%? EmptyWrappedRow
          ]
  querySingleRow $ insertReturningAll sessionsTable vals

sockAddrToIPRange :: SockAddr -> IPRange
sockAddrToIPRange s =
  case fromSockAddr s of
    Nothing -> error "help"
    Just (ip, _) -> case ip of
      IPv4 v4addr -> IPv4Range $ makeAddrRange v4addr 32
      IPv6 v6addr -> IPv6Range $ makeAddrRange v6addr 128
