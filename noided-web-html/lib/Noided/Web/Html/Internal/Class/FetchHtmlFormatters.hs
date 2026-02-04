{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Web.Html.Internal.Class.FetchHtmlFormatters where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State.Lazy as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import Control.Monad.Trans.Class
import Effectful
import Effectful.Dispatch.Dynamic
import Lucid.Base
import Noided.Web.Html.Internal.Type.HtmlFormatter

-- | Monads where you can fetch a 'HtmlFormatters' instance.
class (Monad m) => FetchHtmlFormatters m where
  fetchFormatters :: m HtmlFormatters

instance (FetchHtmlFormatters m) => FetchHtmlFormatters (ReaderT r m) where
  fetchFormatters = lift fetchFormatters

instance (FetchHtmlFormatters m) => FetchHtmlFormatters (LazyState.StateT s m) where
  fetchFormatters = lift fetchFormatters

instance (FetchHtmlFormatters m) => FetchHtmlFormatters (StrictState.StateT s m) where
  fetchFormatters = lift fetchFormatters

instance (FetchHtmlFormatters m, Monoid w) => FetchHtmlFormatters (LazyWriter.WriterT w m) where
  fetchFormatters = lift fetchFormatters

instance (FetchHtmlFormatters m, Monoid w) => FetchHtmlFormatters (StrictWriter.WriterT w m) where
  fetchFormatters = lift fetchFormatters

instance (FetchHtmlFormatters m) => FetchHtmlFormatters (ExceptT e m) where
  fetchFormatters = lift fetchFormatters

instance (FetchHtmlFormatters m) => FetchHtmlFormatters (MaybeT m) where
  fetchFormatters = lift fetchFormatters

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
