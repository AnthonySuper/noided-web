{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Pathname
  (
    -- * Router
    Router,
    singletonRouter,
    insertRouter,
    atRouter,
    alterFRouter,
    traverseRouterWithKey,
    mapRouterWithKey,
    foldMapRouterWithKey,
    foldrRouterWithKey,
    unionRouterWith,
    -- ** Routing
    RouteMatch(..),
    RouteParams(..),
    foldrRouterMatches,
    firstRouterMatch,
    allRouterMatches,
    -- *** Testing Routing
    TemplateMatchResult(..),
    testUrlResult,
    -- ** Merge Routing
    MergeRouter(..),
    -- * Path templates
    PathTemplate(..),
    AppendPathTemplate,
    appendPathTemplate,
    pathTemplateAppendStatic,
    usePathTemplate,
    usePathTemplateParams,
    splitFirstCapture,
    removeFirstCapture,
    TemplateMatchFailureMessage(..),
    TemplateFailure(..),
    matchPathTemplate,
    -- * Templates for individual url pieces
    PieceTemplate(..),
    KnownCapture,
    capPiece,
    matchPieceTemplate,
  )
where

import Noided.Pathname.Internal.Router
import Noided.Pathname.Internal.PathTemplate
import Noided.Pathname.Internal.RouteParams
import Noided.Pathname.Internal.PieceTemplate

newtype MergeRouter contained
  = MergeRouter {getMergeRouter :: Router contained}

instance
  (forall key. Semigroup (contained key)) =>
  Semigroup (MergeRouter contained)
  where
  MergeRouter l <> MergeRouter r = MergeRouter $ unionRouterWith (<>) l r

instance
  (forall key. Semigroup (contained key)) =>
  Monoid (MergeRouter contained)
  where
  mempty = MergeRouter mempty
