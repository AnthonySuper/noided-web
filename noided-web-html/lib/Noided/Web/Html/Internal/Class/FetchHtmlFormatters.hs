{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Web.Html.Internal.Class.FetchHtmlFormatters where

import Control.Monad.Trans.Class
import Effectful
import Effectful.Dispatch.Dynamic
import Lucid.Base
import Noided.Web.Html.Internal.Type.HtmlFormatter

-- | Monads where you can fetch a 'HtmlFormatters' instance.
class (Monad m) => FetchHtmlFormatters m where
  fetchFormatters :: m HtmlFormatters

-- | We can lift formatter fetching through an underlying monad.
instance (FetchHtmlFormatters m) => FetchHtmlFormatters (HtmlT m) where
  fetchFormatters = lift fetchFormatters

data FetchHtmlFormattersE :: Effect where
  FetchHtmlFormattersE :: FetchHtmlFormattersE m HtmlFormatters

type instance DispatchOf FetchHtmlFormattersE = Dynamic

instance (FetchHtmlFormattersE :> es) => FetchHtmlFormatters (Eff es) where
  fetchFormatters = send FetchHtmlFormattersE

runFetchHtmlFormattersE :: HtmlFormatters -> Eff (FetchHtmlFormattersE : es) a -> Eff es a
runFetchHtmlFormattersE msg = interpret $ \_ FetchHtmlFormattersE -> return msg
