{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Page.ItemSpec (spec) where

import Data.Functor.Identity (Identity)
import Data.Time (UTCTime (..), fromGregorian)
import Lucid.Base (HtmlT)
import Noided.Translate
import Noided.Web.Html (TranslationT)
import OptBeer.DB.Ids.ItemId (ItemId (..))
import OptBeer.DB.Ids.OrganizationId (OrganizationId (..))
import OptBeer.DB.Table.Item (Item, ItemF (..))
import OptBeer.DB.Table.Organization (Organization, OrganizationF (..))
import OptBeer.DB.Table.Timestamps (TimestampsF (..))
import OptBeer.DB.Type.Unit (Unit (..))
import OptBeer.DB.Type.UnitCategory (UnitCategory (..))
import OptBeer.Form.Render.SpecHelper
import OptBeer.Page.Item (itemsIndexPage, showItemPage)
import OptBeer.Page.Type
import Test.Hspec

spec :: SpecWith Translations
spec = describe "Item Page Rendering" $ do
  let timestamps =
        Timestamps
          { createdAt = UTCTime (fromGregorian 2026 1 1) 0,
            updatedAt = UTCTime (fromGregorian 2026 1 1) 0
          }
      org =
        Organization
          { id = MkOrganizationId 1,
            name = "Test Org",
            timestamps = timestamps
          }
      item =
        Item
          { id = MkItemId 1,
            organizationId = org.id,
            name = "Test Item",
            description = "A test item description",
            defaultUnit = Gram,
            measureCategory = Mass,
            timestamps = timestamps
          }

  withTranslationsInLocale "en" $ do
    it "renders items index page without bad translations" $ \runner -> do
      let renderAct :: HtmlT (TranslationT Identity) ()
          renderAct = itemsIndexPage org [item]
          soup = runTranslationToSoup runner renderAct
      assertHasNoBadTranslations soup

    it "renders item show page without bad translations" $ \runner -> do
      let renderAct :: HtmlT (TranslationT Identity) ()
          renderAct = showItemPage org item
          soup = runTranslationToSoup runner renderAct
      assertHasNoBadTranslations soup
