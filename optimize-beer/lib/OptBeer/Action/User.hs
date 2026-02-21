{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.User where

import Lucid
import Noided.Pathname
import Noided.Web.Html.FormRender
import Noided.Web.PageAction
import Noided.Web.Response
import OptBeer.Form.Render.CreateUser
import OptBeer.Form.Type.CreateUser
import OptBeer.Routes (newUserPath)

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
