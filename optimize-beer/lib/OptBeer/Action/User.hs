{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.User where

import Control.Monad.Error.Class qualified as MonadError
import Lucid
import OptBeer.Action.Base
import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.User
import OptBeer.DB.Table.UserPassword
import OptBeer.Effect.HashPassword
import OptBeer.Form.Render.CreateUser
import OptBeer.Form.Type.CreateUser
import OptBeer.Form.Validate.CreateUser (createUserValidator, fieldInputToOpaquePassword)
import OptBeer.Routes (newUserPath, usersPath)
import OptBeer.Type.Hashword
import Optics

userActions ::
  ( FetchMessages renderM,
    FetchHtmlFormatters renderM,
    Error BadRequest :> es,
    Error SessionError :> es,
    GetRequestBody :> es,
    HashPassword :> es,
    RunTransaction :> es
  ) =>
  PageRoutes renderM (Eff es)
userActions =
  actGet newUserPath newUserAction
    <> actPost usersPath createUserAction

wrapForm :: (Monad m) => HtmlT m a -> HtmlT m a
wrapForm act = form_ [method_ "post", action_ "/users", class_ "form"] $ do
  res <- act
  div_ [class_ "form-buttons"] $
    button_
      [class_ "button", type_ "submit"]
      "Submit"
  return res

blankFormPage :: (FetchMessages m, FetchHtmlFormatters m) => HtmlT m ()
blankFormPage =
  wrapForm $
    renderFormT createUserRenderer hkdFormEmpty mempty

newUserAction :: (FetchMessages renderM, FetchHtmlFormatters renderM, Monad m) => RouteParams '[] -> m (PageResponse renderM)
newUserAction (RPNil :: RouteParams '[]) =
  return $
    respondPage200 blankFormPage

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
  actor <- querySingleRow $ insertReturningAll actorsTable (singleValue_ (#name :==> mutateVal_ (bindParam validated.name.val) :::%? EmptyWrappedRow))
  let userVals =
        singleValue_
          ( #id
              :==> mutateVal_ (bindParam actor.id)
              :::%? #email
              :==> mutateVal_ (bindParam validated.email.val)
              :::%? EmptyWrappedRow
          )
  user <- querySingleRow $ insertReturningAll usersTable userVals

  let pwVals =
        singleValue_
          ( #userId
              :==> mutateVal_ (bindParam user.id)
              :::%? #passwordDigest
              :==> mutateVal_ (bindParam $ hashwordToPasswordHash hashword)
              :::%? EmptyWrappedRow
          )
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
        respondHKDForm
          wrapForm
          (renderFormT createUserRenderer body)
          err
    Right _ -> return $ RespondRedirect RedirectFound "/"
