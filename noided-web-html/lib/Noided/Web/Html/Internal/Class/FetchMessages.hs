{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Web.Html.Internal.Class.FetchMessages where

import Control.Monad.Trans.Class
import Effectful
import Effectful.Dispatch.Dynamic
import Lucid.Base
import Noided.Translate (Messages)

-- | Monads where you can fetch a 'Messages' instance.
class (Monad m) => FetchMessages m where
  fetchMessages :: m Messages

instance (FetchMessages m) => FetchMessages (HtmlT m) where
  fetchMessages = lift fetchMessages

data FetchMessagesE :: Effect where
  FetchMessagesE :: FetchMessagesE m Messages

type instance DispatchOf FetchMessagesE = Dynamic

instance (FetchMessagesE :> es) => FetchMessages (Eff es) where
  fetchMessages = send FetchMessagesE

runFetchMessagesE :: Messages -> Eff (FetchMessagesE : es) a -> Eff es a
runFetchMessagesE msg = interpret $ \_ FetchMessagesE -> return msg
