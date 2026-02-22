{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Base
  ( -- * Common helpers
    hkdFormBody,

    -- * Common re-exports
    module Effectful,
    module Effectful.Error.Static,
    module Noided.Form,
    module Noided.Form.HKD,
    module Noided.Pathname,
    module Noided.Row,
    module Noided.Sql,
    module Noided.Validation,
    module Noided.Web,
    module Noided.Web.Html.FormRender,
    module OptBeer.Effect.CurrentActor,
  )
where

import Effectful hiding (Limit)
import Effectful.Error.Static
import Noided.Form
import Noided.Form.HKD
import Noided.Pathname
import Noided.Row
import Noided.Sql
import Noided.Validation
import Noided.Web
import Noided.Web.Html.FormRender
import OptBeer.Effect.CurrentActor

-- | Helper to extract an HKD form from a request body.
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
