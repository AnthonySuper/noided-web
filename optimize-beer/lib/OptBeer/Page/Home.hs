{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Page.Home where

import Data.Maybe (isJust)
import Lucid
import Noided.Web.Html
import OptBeer.DB.Table.Actor
import OptBeer.Page.Type

homePage :: Maybe Actor -> Bool -> HtmlT Page ()
homePage mActor hasDefaultOrg =
  div_ [class_ "home-container"] $ do
    div_ [class_ "home-hero"] $ do
      div_ [class_ "home-logo"] "🍺"
      h1_ [class_ "home-title"] $ renderTranslated ["home.hero.title"] mempty
      p_ [class_ "home-tagline"] $ renderTranslated ["home.hero.tagline"] mempty
    
    case (mActor, hasDefaultOrg) of
      (Just _, False) ->
        div_ [class_ "home-org-prompt"] $ do
          h2_ [class_ "home-org-prompt-title"] $ renderTranslated ["home.org_prompt.title"] mempty
          p_ [class_ "home-org-prompt-body"] $ renderTranslated ["home.org_prompt.body"] mempty
          a_ [href_ "/organizations/new", class_ "button home-cta"] $ renderTranslated ["home.org_prompt.cta"] mempty
      _ ->
        div_ [class_ "home-coming-soon"] $ do
          h2_ [class_ "home-coming-soon-title"] $ renderTranslated ["home.coming_soon.title"] mempty
          p_ [class_ "home-coming-soon-body"] $ renderTranslated ["home.coming_soon.body"] mempty
          if isJust mActor
            then return ()
            else a_ [href_ "/users/new", class_ "button home-cta"] $ renderTranslated ["home.coming_soon.cta"] mempty
