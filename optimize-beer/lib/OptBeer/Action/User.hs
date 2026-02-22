{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.User where

import Control.Monad.Error.Class qualified as MonadError
import Data.Aeson
import Data.Text (pack)
import Effectful
import Effectful.Error.Static
import Lucid
import Noided.Form
import Noided.Form.HKD
import Noided.Pathname
import Noided.Row
import Noided.Sql
import Noided.Web
import Noided.Web.Effect
import Noided.Web.Html.FormRender
import Noided.Web.PageAction
import Noided.Web.Response
import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.User
import OptBeer.DB.Table.UserPassword
import OptBeer.Effect.HashPassword
import OptBeer.Error.BadRequest
import OptBeer.Error.BadRequest (BadRequest (BadRequest))
import OptBeer.Form.Render.CreateUser
import OptBeer.Form.Type.CreateUser
import OptBeer.Form.Validate.CreateUser (createUserValidator, fieldInputToOpaquePassword)
import OptBeer.Routes (newUserPath)
import OptBeer.Type.Hashword
import Optics

userActions :: (FetchMessages renderM, FetchHtmlFormatters renderM, Monad actionM) => PageRoutes renderM actionM
userActions = actGet newUserPath newUserAction

wrapForm :: (Monad m) => HtmlT m a -> HtmlT m a
wrapForm = form_ []

blankFormPage :: (FetchMessages m, FetchHtmlFormatters m) => HtmlT m ()
blankFormPage =
  wrapForm $
    renderFormT createUserRenderer emptyCreateUserForm mempty

newUserAction :: (FetchMessages renderM, FetchHtmlFormatters renderM, Monad m) => RouteParams '[] -> m (PageResponse renderM)
newUserAction (RPNil :: RouteParams '[]) =
  return $
    respondPage200 blankFormPage

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

createUserFromForm ::
  ( Error SessionError :> es,
    HashPassword :> es,
    RunTransaction :> es
  ) =>
  CreateUserF FormInput ->
  Eff es (Either (FormErrors (SubformField CreateUserF)) (Actor, User))
createUserFromForm input = do
  let pw = input ^. #password % #val
  let opaque = fieldInputToOpaquePassword pw
  hashword <- hashPassword opaque
  runTransactionEither $ createUserWithHashword hashword input

createUserWithHashword :: Hashword -> CreateUserF FormInput -> TransactM (FormErrors (SubformField CreateUserF)) (Actor, User)
createUserWithHashword (hashword :: Hashword) input = do
  validated <- validateForm createUserValidator input >>= either MonadError.throwError pure
  actor <- querySingleRow $ insertReturningAll actorsTable (values_ [#name :==> mutateVal_ (bindParam validated.name.val) :::%? EmptyWrappedRow])
  let userVals =
        values_
          [ #id
              :==> mutateVal_ (bindParam actor.id)
              :::%? #email
              :==> mutateVal_ (bindParam validated.email.val)
              :::%? EmptyWrappedRow
          ]
  user <- querySingleRow $ insertReturningAll usersTable userVals

  let pwVals =
        values_
          [ #userId
              :==> mutateVal_ (bindParam user.id)
              :::%? #passwordDigest
              :==> mutateVal_ (bindParam $ hashwordToPasswordHash hashword)
              :::%? EmptyWrappedRow
          ]
  _ <- querySingleRow $ insertReturningAll userPasswordsTable pwVals

  return (actor, user)

createUserAction ::
  ( Error BadRequest :> es,
    Error SessionError :> es,
    GetRequestBody :> es,
    HashPassword :> es,
    RunTransaction :> es,
    FetchMessages renderM,
    FetchHtmlFormatters renderM
  ) =>
  RouteParams '[] ->
  Eff es (PageResponse renderM)
createUserAction (RPNil :: RouteParams '[]) = do
  body <- hkdFormBody
  result <- createUserFromForm body
  case result of
    Left err ->
      return $
        RespondFormErrors
          wrapForm
          (renderFormT createUserRenderer body err)
    Right _ -> return $ RespondRedirect RedirectFound "/"
