{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Page.Type where

import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (Reader, ReaderT (..))
import Data.Functor.Identity
import Data.Text (Text)
import Effectful
import GHC.Generics
import Noided.Translate
import Noided.Web.Effect
import Noided.Web.Html
import Noided.Web.PageAction
import Noided.Web.Response
import Optics

-- | A page env.
--
-- Right now this is equivalent
data PageEnv
  = MkPageEnv
  { messages :: !Messages,
    htmlFormatters :: !HtmlFormatters,
    pageTitle :: !(Maybe Text),
    serverEnv :: !ServerEnv,
    assets :: !AssetLinks
  }
  deriving (Generic)

-- | Page rendering monad.
--
-- Provides access to page context.
newtype Page a = MkPage {runPage :: PageEnv -> a}
  deriving (Applicative, Monad, Functor, MonadReader PageEnv) via (Reader PageEnv)

instance FetchMessages Page where
  fetchMessages = view #messages <$> ask

instance FetchHtmlFormatters Page where
  fetchFormatters = view #htmlFormatters <$> ask

readPageTitle :: Eff es (Maybe Text)
readPageTitle = return Nothing

readPageEnv :: (FetchMessagesE :> es, FetchHtmlFormattersE :> es, GetServerEnv :> es, FrontendAssets :> es) => Eff es PageEnv
readPageEnv =
  MkPageEnv
    <$> fetchMessages
    <*> fetchFormatters
    <*> readPageTitle
    <*> getServerEnv
    <*> getAssetLinks "frontend/main.ts"

mapResponsesToPage ::
  ( FetchMessagesE :> es,
    FetchHtmlFormattersE :> es,
    GetServerEnv :> es,
    FrontendAssets :> es
  ) =>
  PageRoutes Page (Eff es) ->
  PageRoutes Identity (Eff es)
mapResponsesToPage = pagesAroundResponse $ \pr -> do
  env <- readPageEnv
  return $
    liftPageResponseRendering (Identity . flip runPage env) pr
