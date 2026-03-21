{-# LANGUAGE OverloadedRecordDot #-}

module OptBeer.Render.Heading where

import Control.Monad
import Data.Foldable
import GHC.Generics
import Lucid
import Noided.Pathname
import OptBeer.DB.Table.Organization
import OptBeer.Routes
import OptBeer.Type.OrganizationIdent (OrganizationIdent (OrganizationById))
import Optics

data HeadingCfg m
  = HeadingCfg
  { breadcrumbs :: [HtmlT m ()],
    title :: HtmlT m (),
    actions :: HtmlT m ()
  }
  deriving (Generic)

deriving via
  (Generically (HeadingCfg m))
  instance
    (Monad m) =>
    Semigroup (HeadingCfg m)

deriving via
  (Generically (HeadingCfg m))
  instance
    (Monad m) =>
    Monoid (HeadingCfg m)

addBreadcrumb :: HtmlT m () -> HeadingCfg m -> HeadingCfg m
addBreadcrumb bc = #breadcrumbs %~ (++ [bc])

emptyHeadingCfg :: (Monad m) => HeadingCfg m
emptyHeadingCfg = mempty

-- | Render a heading, adding a breadcrumb for the organization
renderHeadingOrganization :: (Monad m) => Organization -> HeadingCfg m -> HtmlT m ()
renderHeadingOrganization org =
  renderHeading . (#breadcrumbs %~ (orgBreadcrumb :))
  where
    orgBreadcrumb = do
      a_ [href_ (usePathTemplate showOrganizationPath $ OrganizationById org.id)] $
        toHtml org.name

-- | Render a heading from a config.
renderHeading :: (Monad m) => HeadingCfg m -> HtmlT m ()
renderHeading cfg =
  header_ [class_ "page-heading"] $ do
    unless (hasn't (#breadcrumbs % folded) cfg) $ do
      nav_ [class_ "breadcrumbs"] $ do
        ol_ [class_ "breadcrumb-list"] $ do
          traverse_ (li_ [class_ "breadcrumb-list-item"]) cfg.breadcrumbs
    h1_ [class_ "page-heading-title"] cfg.title
    span_ [class_ "page-heading-actions"] cfg.actions
