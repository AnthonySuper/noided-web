{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Web.Html.Internal.Class.FetchMessages where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Lazy qualified as LazyState
import Control.Monad.Trans.State.Strict qualified as StrictState
import Control.Monad.Trans.Writer.Lazy qualified as LazyWriter
import Control.Monad.Trans.Writer.Strict qualified as StrictWriter
import Effectful
import Effectful.Dispatch.Dynamic
import Lucid.Base
import Noided.Translate (Messages)

-- | Monads where you can fetch a 'Messages' instance.
class (Monad m) => FetchMessages m where
  fetchMessages :: m Messages

instance (FetchMessages m) => FetchMessages (ReaderT r m) where
  fetchMessages = lift fetchMessages

instance (FetchMessages m) => FetchMessages (LazyState.StateT s m) where
  fetchMessages = lift fetchMessages

instance (FetchMessages m) => FetchMessages (StrictState.StateT s m) where
  fetchMessages = lift fetchMessages

instance (FetchMessages m, Monoid w) => FetchMessages (LazyWriter.WriterT w m) where
  fetchMessages = lift fetchMessages

instance (FetchMessages m, Monoid w) => FetchMessages (StrictWriter.WriterT w m) where
  fetchMessages = lift fetchMessages

instance (FetchMessages m) => FetchMessages (ExceptT e m) where
  fetchMessages = lift fetchMessages

instance (FetchMessages m) => FetchMessages (MaybeT m) where
  fetchMessages = lift fetchMessages

instance (FetchMessages m) => FetchMessages (HtmlT m) where
  fetchMessages = lift fetchMessages

data FetchMessagesE :: Effect where
  FetchMessagesE :: FetchMessagesE m Messages

type instance DispatchOf FetchMessagesE = Dynamic

instance (FetchMessagesE :> es) => FetchMessages (Eff es) where
  fetchMessages = send FetchMessagesE

runFetchMessagesE :: Messages -> Eff (FetchMessagesE : es) a -> Eff es a
runFetchMessagesE msg = interpret $ \_ FetchMessagesE -> return msg
