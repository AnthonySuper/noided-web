{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Page.Home where

import Lucid
import OptBeer.Page.Type

homePage :: HtmlT Page ()
homePage =
  div_ [class_ "home-container"] $ do
    div_ [class_ "home-hero"] $ do
      div_ [class_ "home-logo"] "🍺"
      h1_ [class_ "home-title"] "optimize.beer"
      p_ [class_ "home-tagline"] "Take the guesswork out of your brew."
    div_ [class_ "home-coming-soon"] $ do
      h2_ [class_ "home-coming-soon-title"] "Coming Soon"
      p_ [class_ "home-coming-soon-body"] $
        "We're crafting something special. Sign up to be the first to know when we launch."
      a_ [href_ "/users/new", class_ "button home-cta"] "Create an Account"
