{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.Type.FormContextT where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader hiding (ask, asks, local, reader)
import Control.Monad.Writer.Class
import Data.Kind
import Lucid
import Lucid.Base
import Noided.Form.HKD
import Noided.Web.Html.Internal.Class.FetchHtmlFormatters
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Html.Internal.Type.FormContext
import Optics.Core

-- | Monad transformer to render forms.
--
-- Note that this is a reader monad, but it /does not/ derive @MonadReader@.
-- This is so it can lift the @MonadReader@ definitions from inner monads.
type FormContextT :: (Type -> Type) -> Type -> Type
newtype FormContextT m a
  = FormContextT {runFormContextT :: FormContext -> m a}
  deriving (Functor, Applicative, Monad) via (ReaderT FormContext m)
  deriving (MonadTrans) via (ReaderT FormContext)

instance (MonadReader r m) => MonadReader r (FormContextT m) where
  local f (FormContextT m) = FormContextT $ \ctx -> local f (m ctx)
  ask = lift ask

deriving via (ReaderT FormContext m) instance (MonadState s m) => MonadState s (FormContextT m)
deriving via (ReaderT FormContext m) instance (MonadError e m) => MonadError e (FormContextT m)
deriving via (ReaderT FormContext m) instance (MonadIO m) => MonadIO (FormContextT m)
deriving via (ReaderT FormContext m) instance (MonadWriter w m) => MonadWriter w (FormContextT m)

instance (FetchMessages m) => FetchMessages (FormContextT m) where
  fetchMessages = lift fetchMessages

instance (FetchHtmlFormatters m) => FetchHtmlFormatters (FormContextT m) where
  fetchFormatters = lift fetchFormatters

runForm :: (Monad n) => HtmlT (FormContextT n) b -> HtmlT n b
runForm = hoistHtmlT (`runFormContextT` mempty)

askFormContext :: (Applicative m) => FormContextT m FormContext
askFormContext = FormContextT pure

localFormContext :: (FormContext -> FormContext) -> FormContextT m a -> FormContextT m a
localFormContext f fr = FormContextT (runFormContextT fr . f)

-- | Monad transformer to render fields.
--
-- Note that this is a reader monad, but it /does not/ derive @MonadReader@.
-- This is so it can lift the @MonadReader@ definitions from inner monads, as the user will often
-- want to render in some reader monad that has context like the current user and such.
--
type FieldRendererT :: HKDFieldType -> (Type -> Type) -> Type -> Type
newtype FieldRendererT field m a
  = FieldRendererT {runFieldRendererT :: FieldContext field -> m a}
  deriving (Functor, Applicative, Monad) via (ReaderT (FieldContext field) m)
  deriving (MonadTrans) via (ReaderT (FieldContext field))

instance (MonadReader r m) => MonadReader r (FieldRendererT field m) where
  local f (FieldRendererT m) = FieldRendererT $ \ctx -> local f (m ctx)
  ask = lift ask

deriving via (ReaderT (FieldContext field) m) instance (MonadState s m) => MonadState s (FieldRendererT field m)
deriving via (ReaderT (FieldContext field) m) instance (MonadError e m) => MonadError e (FieldRendererT field m)
deriving via (ReaderT (FieldContext field) m) instance (MonadIO m) => MonadIO (FieldRendererT field m)
deriving via (ReaderT (FieldContext field) m) instance (MonadWriter w m) => MonadWriter w (FieldRendererT field m)

instance (FetchMessages m) => FetchMessages (FieldRendererT field m) where
  fetchMessages = lift fetchMessages

instance (FetchHtmlFormatters m) => FetchHtmlFormatters (FieldRendererT field m) where
  fetchFormatters = lift fetchFormatters

askFieldContext :: (Applicative m) => FieldRendererT field m (FieldContext field)
askFieldContext = FieldRendererT pure

localFieldContext ::
  (FieldContext field -> FieldContext field) ->
  FieldRendererT field m a ->
  FieldRendererT field m a
localFieldContext f act = FieldRendererT $ runFieldRendererT act . f

fieldRendererToForm :: RenderingContext field -> FieldRendererT field m a -> FormContextT m a
fieldRendererToForm fieldCtx fr = FormContextT $ \formCtx -> do
  let fieldContext = FieldCtx formCtx fieldCtx
  runFieldRendererT fr fieldContext

formRendererToField :: FormContextT m a -> FieldRendererT field m a
formRendererToField act = FieldRendererT $ \formCtx ->
  runFormContextT act (formCtx ^. #baseContext)
