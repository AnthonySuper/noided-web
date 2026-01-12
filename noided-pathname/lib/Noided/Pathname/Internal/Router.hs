{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Pathname.Internal.Router where

import Control.DeepSeq (NFData (..))
import Data.Dependent.Map qualified as DM
import Data.Dependent.Sum
import Data.Functor.Const
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare
import Data.GADT.Show
import Data.HKD
import Data.HashMap.Strict qualified as HM
import Data.Kind (Type)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (Endo (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality ((:~:) (Refl))
import GHC.Generics
import Noided.Pathname.Internal.PathCaptures
import Noided.Pathname.Internal.PathTemplate
import Noided.Pathname.Internal.PieceTemplate
import Noided.Pathname.Internal.RouteParams
import Optics.Core
import Test.Inspection
import Unsafe.Coerce

newtype RoutingCap t = RouteCap {getRouteCap :: PieceTemplate (Just t)}
  deriving (Show, Read, Eq, Ord, NFData)

instance GEq RoutingCap where
  geq (RouteCap l) (RouteCap r) = do
    Refl <- geq l r
    Just Refl

instance GCompare RoutingCap where
  gcompare (RouteCap l) (RouteCap r) =
    case gcompare l r of
      GLT -> GLT
      GGT -> GGT
      GEQ -> GEQ

instance GShow RoutingCap where
  gshowsPrec = defaultGshowsPrec

data AfterCapture captures contained singleCap where
  AfterCap ::
    (KnownPathCaptures captures) =>
    !(RouterOf (AppendPathCaptures captures '[singleCap]) contained) ->
    AfterCapture captures contained singleCap

-- | Prove that capturing works the way we think it does.
-- This proof is erased at runtime via the use of rewrite rules.
proveAfterCaptureAppending ::
  forall a b captures contained singleCap.
  AfterCapture captures contained singleCap ->
  AppendPathCaptures (AppendPathCaptures captures a) b
    :~: AppendPathCaptures captures (AppendPathCaptures a b)
proveAfterCaptureAppending (AfterCap _) = proveAppendIsAssociativeSing @_ @a @b (pathCaptureSing @captures)
{-# NOINLINE [1] proveAfterCaptureAppending #-}

{-# RULES
"eraseProof/proveAfterCaptureAppending" [~1] forall cap.
  proveAfterCaptureAppending cap =
    cap `seq` unsafeCoerce Refl
  #-}

unionAfterCaptureWith ::
  (forall caps. contained caps -> contained caps -> contained caps) ->
  AfterCapture captures contained singleCap ->
  AfterCapture captures contained singleCap ->
  AfterCapture captures contained singleCap
unionAfterCaptureWith combine (AfterCap l) (AfterCap r) =
  AfterCap $ unionRouterOfWith combine l r

type CaptureMap captures contained = DM.DMap RoutingCap (AfterCapture captures contained)

unionCaptureMapWith ::
  (forall caps. contained caps -> contained caps -> contained caps) ->
  CaptureMap captures contained ->
  CaptureMap captures contained ->
  CaptureMap captures contained
unionCaptureMapWith f = DM.unionWithKey (\_ -> unionAfterCaptureWith f)

foldrCaptureMap ::
  forall captures contained r.
  (AppendPathCaptures captures '[] ~ captures) =>
  (forall toAppend. RouteParams toAppend -> RouteParams (AppendPathCaptures captures toAppend)) ->
  Text ->
  [Text] ->
  (RouteMatch contained -> r -> r) ->
  r ->
  CaptureMap captures contained ->
  r
foldrCaptureMap buildParams initParam restParams fold = DM.foldrWithKey f
  where
    f :: forall v. RoutingCap v -> AfterCapture captures contained v -> r -> r
    f (RouteCap cap) afterCap@(AfterCap r) rest =
      case matchPieceTemplate cap initParam of
        Nothing -> rest
        Just c ->
          case proveAfterCaptureAppending @'[v] @'[] afterCap of
            Refl ->
              foldrRouterOfMatches
                ( \(toAppend :: RouteParams newParams) ->
                    case proveAfterCaptureAppending @'[v] @newParams afterCap of
                      Refl ->
                        buildParams (c :-$ toAppend)
                )
                restParams
                fold
                rest
                r

inspectFoldrCaptureMapErasesProof :: Result
inspectFoldrCaptureMapErasesProof = $(inspectTest ('foldrCaptureMap `doesNotUse` 'proveAfterCaptureAppending))

traverseCaptureMapWithKey ::
  (Applicative f) =>
  PathTemplate captures ->
  (forall caps. PathTemplate caps -> contained caps -> f (contained' caps)) ->
  CaptureMap captures contained ->
  f (CaptureMap captures contained')
traverseCaptureMapWithKey pt f = DM.traverseWithKey $ \(RouteCap sp) (AfterCap cm) ->
  AfterCap <$> traverseRouterOfWithKey (appendPathTemplate pt $ sp :/ PathEnd) f cm

type StaticMap captures contained = HM.HashMap Text (RouterOf captures contained)

unionStaticMapWith ::
  (forall caps. contained caps -> contained caps -> contained caps) ->
  StaticMap captures contained ->
  StaticMap captures contained ->
  StaticMap captures contained
unionStaticMapWith f = HM.unionWith (unionRouterOfWith f)

foldrStaticMap ::
  (AppendPathCaptures captures '[] ~ captures) =>
  (forall toAppend. RouteParams toAppend -> RouteParams (AppendPathCaptures captures toAppend)) ->
  Text ->
  [Text] ->
  (RouteMatch contained -> r -> r) ->
  r ->
  StaticMap captures contained ->
  r
foldrStaticMap buildRouteParams lm restParam fold base sm =
  case HM.lookup lm sm of
    Nothing -> base
    Just t ->
      foldrRouterOfMatches
        buildRouteParams
        restParam
        fold
        base
        t

traverseStaticMapWithKey ::
  (Applicative f) =>
  PathTemplate captures ->
  (forall caps. PathTemplate caps -> contained caps -> f (contained' caps)) ->
  StaticMap captures contained ->
  f (StaticMap captures contained')
traverseStaticMapWithKey pt f sm =
  case provePathTemplateAppendNothingDoesNothing pt of
    Refl ->
      HM.traverseWithKey
        (\tk -> traverseRouterOfWithKey (appendPathTemplate pt (StaticPiece tk :/ PathEnd)) f)
        sm

type RouterOf :: [Type] -> ([Type] -> Type) -> Type
data RouterOf captures contained
  = RouteOf
  { leaf :: {-# UNPACK #-} !(Maybe (contained captures)),
    statics :: !(StaticMap captures contained),
    captures :: !(CaptureMap captures contained)
  }
  deriving (Generic)

instance Semigroup (RouterOf captures contained) where
  (<>) = unionRouterOfWith const

instance Monoid (RouterOf captures contained) where
  mempty = RouteOf Nothing mempty mempty

-- | NFData instance for AfterCapture
rnfAfterCapture ::
  (forall key. NFData (contained key)) =>
  AfterCapture captures contained singleCap ->
  ()
rnfAfterCapture (AfterCap r) = rnfRouterOf r

-- | NFData instance for RouterOf
rnfRouterOf ::
  (forall key. NFData (contained key)) =>
  RouterOf captures contained ->
  ()
rnfRouterOf (RouteOf leaf statics captures) =
  rnf leaf `seq`
    HM.foldr' (\v acc -> rnfRouterOf v `seq` acc) () statics `seq`
      DM.foldrWithKey (\k v acc -> rnf k `seq` rnfAfterCapture v `seq` acc) () captures

instance (forall key. NFData (contained key)) => NFData (RouterOf captures contained) where
  rnf = rnfRouterOf

routerEraseEmpty :: RouterOf captures contained -> Maybe (RouterOf captures contained)
routerEraseEmpty router
  | isNothing router.leaf && HM.null router.statics && DM.null router.captures = Nothing
  | otherwise = Just router

alterFRouterOfMaybe ::
  (Functor f) =>
  PathTemplate suffix ->
  (Maybe (contained suffix) -> f (Maybe (contained suffix))) ->
  RouterOf '[] contained ->
  f (Maybe (RouterOf '[] contained))
alterFRouterOfMaybe = alterFRouterOfMaybe' PathEnd

alterFRouterOfMaybe' ::
  forall prefix suffix contained f.
  (Functor f) =>
  PathTemplate prefix ->
  PathTemplate suffix ->
  (Maybe (contained (AppendPathCaptures prefix suffix)) -> f (Maybe (contained (AppendPathCaptures prefix suffix)))) ->
  RouterOf prefix contained ->
  f (Maybe (RouterOf prefix contained))
alterFRouterOfMaybe' prefixKey suffixKey modify router =
  case suffixKey of
    PathEnd ->
      case provePathTemplateAppendNothingDoesNothing prefixKey of
        Refl ->
          modify router.leaf <&> \newLeaf ->
            routerEraseEmpty $
              router {leaf = newLeaf}
    (headPiece :/ (rest :: PathTemplate restCaps)) ->
      case headPiece of
        (StaticPiece k) ->
          let altering = alterFRouterOfMaybe' prefixKey rest modify . fromMaybe mempty
              newMap =
                HM.alterF
                  altering
                  k
                  router.statics
           in newMap <&> \newStatics -> routerEraseEmpty $ router {statics = newStatics}
        c@(CapPiece @headCap) ->
          case provePathTemplateAppendIsAssociative @_ @'[headCap] @restCaps prefixKey of
            Refl ->
              let newPrefix = appendPathTemplate prefixKey (c :/ PathEnd)
               in DM.alterF
                    (RouteCap c)
                    ( \case
                        Nothing ->
                          withKnownPathCaptures
                            (pathTemplateToCapturesSing prefixKey)
                            (fmap AfterCap <$> alterFRouterOfMaybe' newPrefix rest modify mempty)
                        Just (AfterCap r) -> fmap AfterCap <$> alterFRouterOfMaybe' newPrefix rest modify r
                    )
                    router.captures
                    <&> \newCaptures ->
                      routerEraseEmpty $ router {captures = newCaptures}

unionRouterOfWith ::
  (forall caps. contained caps -> contained caps -> contained caps) ->
  RouterOf captures contained ->
  RouterOf captures contained ->
  RouterOf captures contained
unionRouterOfWith f lhs rhs =
  RouteOf
    { leaf = combinedLeaf,
      statics = unionStaticMapWith f lhs.statics rhs.statics,
      captures = unionCaptureMapWith f lhs.captures rhs.captures
    }
  where
    combinedLeaf =
      case lhs.leaf of
        Nothing -> rhs.leaf
        Just l ->
          case rhs.leaf of
            Nothing -> lhs.leaf
            Just r -> Just $ f l r

-- | A router match gives you a type-safe parsed list of URL parameters, and some route that uses them.
-- This type is equivalent to @ DSum RouteParams contained @, but we provide our own version here
-- in case we want to extend its functionality in the future.
data RouteMatch contained where
  RouteMatched ::
    RouteParams captures ->
    contained captures ->
    RouteMatch contained

foldrRouterOfMatches ::
  forall r captures contained.
  (AppendPathCaptures captures '[] ~ captures) =>
  (forall toAppend. RouteParams toAppend -> RouteParams (AppendPathCaptures captures toAppend)) ->
  [Text] ->
  (RouteMatch contained -> r -> r) ->
  r ->
  RouterOf captures contained ->
  r
foldrRouterOfMatches buildRouteParams pathPieces fold initial router =
  case pathPieces of
    [] -> baseCase
    [x]
      | Text.null x -> baseCase
    (x : xs) ->
      let staticBase = foldrStaticMap buildRouteParams x xs fold initial router.statics
       in foldrCaptureMap buildRouteParams x xs fold staticBase router.captures
  where
    baseCase =
      case router.leaf of
        Nothing -> initial
        Just r -> fold (RouteMatched builtParams r) initial
    builtParams :: RouteParams captures
    builtParams = buildRouteParams RPNil

traverseRouterOfWithKey ::
  (Applicative f) =>
  PathTemplate captures ->
  (forall key. PathTemplate key -> contained key -> f (contained' key)) ->
  RouterOf captures contained ->
  f (RouterOf captures contained')
traverseRouterOfWithKey pt f router =
  RouteOf
    <$> traverse (f pt) router.leaf
    <*> traverseStaticMapWithKey pt f router.statics
    <*> traverseCaptureMapWithKey pt f router.captures

-- | A dependent-router of some contained type.
-- This router maps URLs to some routed type 'contained', which is most commonly a controller action of some variety.
-- It behaves similarly to a map, with the ability to insert and remove keys, but can also perform *routing*.
-- This will give you one or more routes, along with the route params for them, parsed from the URL.
-- See 'firstRouterMatch', 'foldrRouteMatches', and 'allRouterMatches' for this functionality.
type Router :: ([Type] -> Type) -> Type
newtype Router contained = MkRouter (RouterOf '[] contained)

instance Semigroup (Router contained) where
  (MkRouter l) <> (MkRouter r) = MkRouter (l <> r)

instance Monoid (Router contained) where
  mempty = MkRouter mempty

instance FFunctor Router where
  ffmap f = mapRouterWithKey (const f)

instance FFoldable Router where
  ffoldMap f = foldMapRouterWithKey (const f)

instance FTraversable Router where
  ftraverse f = traverseRouterWithKey (const f)

instance (forall key. NFData (contained key)) => NFData (Router contained) where
  rnf (MkRouter r) = rnf r

-- | Determines if a router is empty
routerNull :: Router contained -> Bool
routerNull (MkRouter r) =
  isNothing r.leaf
    && DM.null r.captures
    && HM.null r.statics

-- | Transform all routes in a given applicative context.
traverseRouterWithKey :: (Applicative f) => (forall caps. PathTemplate caps -> contained caps -> f (contained' caps)) -> Router contained -> f (Router contained')
traverseRouterWithKey f (MkRouter r) = MkRouter <$> traverseRouterOfWithKey PathEnd f r

foldMapRouterWithKey :: (Monoid m) => (forall caps. PathTemplate caps -> contained caps -> m) -> Router contained -> m
foldMapRouterWithKey f = getConst . traverseRouterWithKey (\k v -> Const $ f k v)

mapRouterWithKey :: (forall caps. PathTemplate caps -> contained caps -> contained' caps) -> Router contained -> Router contained'
mapRouterWithKey f = runIdentity . traverseRouterWithKey (\k v -> Identity $ f k v)

foldrRouterWithKey :: (forall caps. PathTemplate caps -> contained caps -> r -> r) -> r -> Router contained -> r
foldrRouterWithKey f initialVal r = appEndo (foldMapRouterWithKey (\k v -> Endo (f k v)) r) initialVal

routerToRoutes :: Router contained -> [DSum PathTemplate contained]
routerToRoutes = foldrRouterWithKey (\k v -> ((k :=> v) :)) []

traverseRouterRoutes_ :: (Applicative f) => (DSum PathTemplate contained -> f u) -> Router contained -> f ()
traverseRouterRoutes_ f = foldrRouterWithKey (\k v r -> f (k :=> v) *> r) (pure ())

routerRoutes :: Fold (Router contained) (DSum PathTemplate contained)
routerRoutes = foldVL traverseRouterRoutes_

-- | Alter the value of a particular key in a functor context.
-- This is used to implement 'atRouter', as well as many other functions.
alterFRouter :: (Functor f) => PathTemplate key -> (Maybe (contained key) -> f (Maybe (contained key))) -> Router contained -> f (Router contained)
alterFRouter key alter (MkRouter r) = MkRouter . fromMaybe mempty <$> alterFRouterOfMaybe key alter r

atRouter :: PathTemplate key -> Lens (Router contained) (Router contained) (Maybe (contained key)) (Maybe (contained key))
atRouter key = lensVL (alterFRouter key)

insertRouter :: PathTemplate key -> contained key -> Router contained -> Router contained
insertRouter key v = atRouter key ?~ v

singletonRouter :: PathTemplate key -> contained key -> Router contained
singletonRouter k v = insertRouter k v mempty

deleteRouter :: PathTemplate key -> Router contained -> Router contained
deleteRouter k = atRouter k .~ Nothing

unionRouterWith ::
  (forall caps. contained caps -> contained caps -> contained caps) ->
  Router contained ->
  Router contained ->
  Router contained
unionRouterWith combine (MkRouter l) (MkRouter r) =
  MkRouter $
    unionRouterOfWith combine l r

foldrRouterMatches ::
  -- | A list of URL parameters
  [Text] ->
  -- | Fold function for a route match
  (RouteMatch contained -> r -> r) ->
  -- | Initial value
  r ->
  -- | Router
  Router contained ->
  -- | Ending value
  r
foldrRouterMatches parts f base (MkRouter r) = foldrRouterOfMatches id parts f base r

firstRouterMatch :: [Text] -> Router contained -> Maybe (RouteMatch contained)
firstRouterMatch parts = foldrRouterMatches parts (const . Just) Nothing

allRouterMatches :: [Text] -> Router contained -> [RouteMatch contained]
allRouterMatches parts = foldrRouterMatches parts (:) []

data TemplateMatchResult contained where
  TemplateMatch ::
    PathTemplate caps ->
    contained caps ->
    Either TemplateFailure (RouteParams caps) ->
    TemplateMatchResult contained

testUrlResult :: [Text] -> Router contained -> [TemplateMatchResult contained]
testUrlResult t =
  foldrRouterWithKey
    (\k v -> (TemplateMatch k v (matchPathTemplate t k) :))
    []
