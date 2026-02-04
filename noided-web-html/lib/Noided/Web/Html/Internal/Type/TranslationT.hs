{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.Type.TranslationT where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Kind
import GHC.Generics
import Noided.Translate
import Noided.Web.Html.Internal.Class.FetchHtmlFormatters
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Html.Internal.Type.HtmlFormatter

data TranslationEnv
  = TranslateEnv
  { messages :: !Messages,
    formatters :: !HtmlFormatters
  }
  deriving (Generic)

-- | Extremely basic monad transformer that provides translations for rendering purposes.
type TranslationT :: (Type -> Type) -> Type -> Type
newtype TranslationT m a = TranslationT {getTranslationT :: TranslationEnv -> m a}
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT TranslationEnv m)
  deriving (MonadTrans) via (ReaderT TranslationEnv)

instance (Monad m) => FetchHtmlFormatters (TranslationT m) where
  fetchFormatters = TranslationT $ return . formatters

instance (Monad m) => FetchMessages (TranslationT m) where
  fetchMessages = TranslationT $ return . messages

translateFromEnv :: (FetchHtmlFormatters m1, FetchMessages m1) => TranslationT m2 a -> m1 (m2 a)
translateFromEnv act = do
  formatters <- fetchFormatters
  messages <- fetchMessages
  return (getTranslationT act $ TranslateEnv messages formatters)

unwrapTranslationT :: (FetchHtmlFormatters m, FetchMessages m) => TranslationT m a -> m a
unwrapTranslationT = join . translateFromEnv
