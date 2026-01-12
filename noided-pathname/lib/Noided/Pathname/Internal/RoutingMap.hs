{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_HADDOCK not-home #-}

module Noided.Pathname.Internal.RoutingMap where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Coerce
import Data.Dependent.Map qualified as DMap
import Data.Functor
import Data.Functor.Identity
import Data.HKD
import Data.HashMap.Strict qualified as Map
import Data.Kind (Type)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Base (build)
import GHC.Generics
import Noided.Pathname.Internal.PathTemplate
import Noided.Pathname.Internal.PieceTemplate
import Noided.Pathname.Internal.RouteParams
import Noided.Pathname.Internal.RoutingCapture
import Optics.Core (Lens', lensVL, (&), (.~))

data RoutingMapLeaf (key :: [Type]) (contained :: [Type] -> Type)
  = RouteLeaf
  { value :: {-# UNPACK #-} !(Maybe (contained key)),
    captures :: !(DMap.DMap RoutingCapture (AfterCapture key contained)),
    statics :: !(Map.HashMap Text (RoutingMapLeaf key contained))
  }
  deriving (Generic)

unionRoutingMapLeafWith ::
  forall container parentKey.
  (forall key. container key -> container key -> container key) ->
  RoutingMapLeaf parentKey container ->
  RoutingMapLeaf parentKey container ->
  RoutingMapLeaf parentKey container
unionRoutingMapLeafWith f lhs rhs =
  RouteLeaf
    newVal
    newCaptures
    newStatics
  where
    newCaptures =
      DMap.unionWithKey
        (\_ (AfterCap l) (AfterCap r) -> AfterCap $ unionRoutingMapLeafWith f l r)
        lhs.captures
        rhs.captures
    newStatics =
      Map.unionWith (unionRoutingMapLeafWith f) lhs.statics rhs.statics
    newVal =
      case (lhs.value, rhs.value) of
        (Nothing, Nothing) -> Nothing
        (Just a, Nothing) -> Just a
        (Nothing, Just b) -> Just b
        (Just a, Just b) -> Just $ f a b

fzipRoutingMapLeafWith ::
  forall lhs rhs res someKey.
  (forall key. lhs key -> rhs key -> res key) ->
  RoutingMapLeaf someKey lhs ->
  RoutingMapLeaf someKey rhs ->
  RoutingMapLeaf someKey res
fzipRoutingMapLeafWith f lhs rhs =
  RouteLeaf
    newValue
    newCaps
    newStatics
  where
    newStatics = Map.mapMaybeWithKey mergeWithText lhs.statics
    newCaps = DMap.mapMaybeWithKey mergeWithKey lhs.captures
    mergeWithKey ::
      forall otherKey.
      RoutingCapture otherKey ->
      AfterCapture someKey lhs otherKey ->
      Maybe (AfterCapture someKey res otherKey)
    mergeWithKey k (AfterCap l) = do
      AfterCap r <- DMap.lookup k rhs.captures
      pure $ AfterCap $ fzipRoutingMapLeafWith f l r
    newValue = f <$> lhs.value <*> rhs.value
    mergeWithText ::
      Text ->
      RoutingMapLeaf someKey lhs ->
      Maybe (RoutingMapLeaf someKey res)
    mergeWithText t l = do
      r <- Map.lookup t rhs.statics
      pure $ fzipRoutingMapLeafWith f l r
{-# INLINEABLE fzipRoutingMapLeafWith #-}

rnfRoutingMapLeaf ::
  (forall key. (NFData (contained key))) =>
  RoutingMapLeaf k contained ->
  ()
rnfRoutingMapLeaf (RouteLeaf val c s) =
  rnf val `seq`
    DMap.foldrWithKey (\k v r -> rnf k `seq` rnfAfterCapture v `seq` r) () c `seq`
      Map.foldrWithKey (\k v r -> rnf k `seq` rnfRoutingMapLeaf v `seq` r) () s

ensureLeafPresent :: RoutingMapLeaf key contained -> Maybe (RoutingMapLeaf key contained)
ensureLeafPresent val
  | isNothing val.value && DMap.null val.captures && Map.null val.statics = Nothing
  | otherwise = pure val
{-# INLINEABLE ensureLeafPresent #-}

instance Semigroup (RoutingMapLeaf key contained) where
  lhs <> rhs =
    RouteLeaf
      (lhs.value <|> rhs.value)
      (DMap.unionWithKey (const (<>)) lhs.captures rhs.captures)
      (Map.unionWith (<>) lhs.statics rhs.statics)

instance Monoid (RoutingMapLeaf key contained) where
  mempty = RouteLeaf Nothing mempty mempty

data FoundRoute contained where
  Routed ::
    RouteParams types ->
    contained types ->
    FoundRoute contained

foldrRouteLeafMatches ::
  forall key contained b.
  RouteParams key ->
  [Text] ->
  (FoundRoute contained -> b -> b) ->
  b ->
  RoutingMapLeaf key contained ->
  b
foldrRouteLeafMatches captures vals foldSum def rm =
  case vals of
    [] -> findDefault
    (x : xs) ->
      let fromCaptures = DMap.foldrWithKey (foldCapturing x xs) def rm.captures
       in (maybe fromCaptures (foldrRouteLeafMatches captures xs foldSum fromCaptures) $ Map.lookup x rm.statics)
  where
    findDefault = maybe def (\val -> foldSum (Routed captures val) def) rm.value
    foldCapturing :: forall cap. Text -> [Text] -> RoutingCapture cap -> AfterCapture key contained cap -> b -> b
    foldCapturing h t rc (AfterCap newMap) b =
      maybe b (\captured -> foldrRouteLeafMatches (captured :-$ captures) t foldSum b newMap) $
        parseWithCapture rc h
{-# INLINEABLE foldrRouteLeafMatches #-}

instance (forall key. (NFData (contained key))) => NFData (RoutingMapLeaf k contained) where
  rnf = rnfRoutingMapLeaf

newtype AfterCapture key contained val where
  AfterCap ::
    RoutingMapLeaf (val ': key) contained ->
    AfterCapture key contained val

rnfAfterCapture :: (forall key. (NFData (contained key))) => AfterCapture k contained value -> ()
rnfAfterCapture (AfterCap rl) = rnfRoutingMapLeaf rl

instance Semigroup (AfterCapture key contained val) where
  AfterCap lhs <> AfterCap rhs =
    AfterCap (lhs <> rhs)

instance (forall key. NFData (contained key)) => NFData (AfterCapture k contained value) where
  rnf = rnfAfterCapture

newtype RoutingMap contained = RouteTrunk (RoutingMapLeaf '[] contained)
  deriving (Semigroup, Monoid) via (RoutingMapLeaf '[] contained)

instance (forall key. NFData (contained key)) => NFData (RoutingMap contained) where
  rnf (RouteTrunk t) = rnf t

instance FFunctor RoutingMap where
  ffmap = ffmapDefault

instance FFoldable RoutingMap where
  ffoldMap = ffoldMapDefault

instance FTraversable RoutingMap where
  ftraverse f =
    traverseRoutingMapWithKey (const f)

-- | Note: values that are not in both sides of the map will be dropped.
instance FZip RoutingMap where
  fzipWith f (RouteTrunk l) (RouteTrunk r) =
    RouteTrunk $ fzipRoutingMapLeafWith f l r

buildLeaf :: PathTemplate path -> contained path -> RoutingMapLeaf path contained
buildLeaf _ c = RouteLeaf (Just c) mempty mempty

flattenLeaf :: PathTemplate path -> RoutingMapLeaf path contained -> RoutingMapLeaf '[] contained
flattenLeaf PathEnd l = l
flattenLeaf (h :/ t) l =
  flattenLeaf t $
    case h of
      CapPiece @a ->
        RouteLeaf
          Nothing
          (DMap.singleton (RouteCap @a) (AfterCap l))
          mempty
      StaticPiece txt ->
        RouteLeaf
          Nothing
          mempty
          (Map.singleton txt l)

{-
flattenLeaf (StaticComponent txt r) l =
  flattenLeaf r $ RouteLeaf Nothing mempty (Map.singleton txt l)
  -}

flattenAlterF ::
  forall f path contained.
  (Functor f) =>
  PathTemplate path ->
  (Maybe (RoutingMapLeaf path contained) -> f (Maybe (RoutingMapLeaf path contained))) ->
  Maybe (RoutingMapLeaf '[] contained) ->
  f (Maybe (RoutingMapLeaf '[] contained))
flattenAlterF PathEnd f = f
flattenAlterF (h :/ t) f =
  flattenAlterF t $ \case
    Nothing ->
      case h of
        StaticPiece txt ->
          fmap (RouteLeaf Nothing mempty . Map.singleton txt) <$> f Nothing
        a@CapPiece ->
          fmap (buildSingleFromCap a) <$> f Nothing
    Just l ->
      case h of
        StaticPiece txt ->
          Map.alterF f txt l.statics <&> \hm -> do
            guard $ not $ Map.null hm
            pure $ l & #statics .~ hm
        (CapPiece @cap) ->
          DMap.alterF (RouteCap @cap) (fmap coerce . f . coerce) l.captures <&> \hm -> do
            guard $ not $ DMap.null hm
            pure $ l & #captures .~ hm
  where
    buildSingleFromCap :: forall a r. PieceTemplate (Just a) -> RoutingMapLeaf (a ': r) contained -> RoutingMapLeaf r contained
    buildSingleFromCap CapPiece v = RouteLeaf Nothing (DMap.singleton (RouteCap @a) (AfterCap v)) mempty

alterFLeaf ::
  (Functor f) =>
  PathTemplate path ->
  (Maybe (contained path) -> f (Maybe (contained path))) ->
  RoutingMapLeaf '[] contained ->
  f (RoutingMapLeaf '[] contained)
alterFLeaf rk alter = fmap (fromMaybe mempty) . flattenAlterF rk inner . Just
  where
    inner Nothing = fmap (\x -> RouteLeaf (Just x) mempty mempty) <$> alter Nothing
    inner (Just val) =
      ensureLeafPresent . (\v -> RouteLeaf v val.captures val.statics)
        <$> alter val.value

atRoutingMap :: PathTemplate path -> Lens' (RoutingMap contained) (Maybe (contained path))
atRoutingMap k = lensVL (alterFRoutingMap k)

alterFRoutingMap ::
  (Functor f) =>
  PathTemplate path ->
  (Maybe (contained path) -> f (Maybe (contained path))) ->
  RoutingMap contained ->
  f (RoutingMap contained)
alterFRoutingMap rk alt (RouteTrunk trunk) = RouteTrunk <$> alterFLeaf rk alt trunk

alterRoutingMap ::
  PathTemplate path ->
  (Maybe (contained path) -> Maybe (contained path)) ->
  RoutingMap contained ->
  RoutingMap contained
alterRoutingMap rk alt rm =
  runIdentity $ alterFRoutingMap rk (Identity . alt) rm

singletonRoutingMap :: PathTemplate path -> contained path -> RoutingMap contained
singletonRoutingMap pk v = RouteTrunk $ flattenLeaf pk (buildLeaf pk v)
{-# NOINLINE [1] singletonRoutingMap #-}

insertRoutingMap ::
  PathTemplate path ->
  contained path ->
  RoutingMap contained ->
  RoutingMap contained
insertRoutingMap rk contained = alterRoutingMap rk (const $ Just contained)

traverseRoutingMapWithKey ::
  forall contained contained' f.
  (Applicative f) =>
  (forall key. PathTemplate key -> contained key -> f (contained' key)) ->
  RoutingMap contained ->
  f (RoutingMap contained')
traverseRoutingMapWithKey f (RouteTrunk trunk) = RouteTrunk <$> go PathEnd trunk
  where
    go :: forall k. PathTemplate k -> RoutingMapLeaf k contained -> f (RoutingMapLeaf k contained')
    go k rk =
      RouteLeaf
        <$> traverse (f k) rk.value
        <*> DMap.traverseWithKey (\RouteCap (AfterCap l) -> AfterCap <$> go (CapPiece :/ k) l) rk.captures
        <*> Map.traverseWithKey (\txt v -> go (StaticPiece txt :/ k) v) rk.statics

foldMapRoutingMapWithKey ::
  forall contained m.
  (Monoid m) =>
  (forall key. PathTemplate key -> contained key -> m) ->
  RoutingMap contained ->
  m
foldMapRoutingMapWithKey f = getConst . traverseRoutingMapWithKey inner
  where
    inner :: forall key. PathTemplate key -> contained key -> Const m (contained key)
    inner k v = Const $ f k v

subrouteRoutingMap ::
  PathTemplate parentCaps ->
  (forall childCaps. contained childCaps -> contained' (AppendPathCaptures parentCaps childCaps)) ->
  RoutingMap contained ->
  RoutingMap contained'
subrouteRoutingMap pt f = foldMapRoutingMapWithKey $ \k v ->
  let newKey = appendPathTemplate pt k
   in singletonRoutingMap newKey (f v)

mapRoutingMapWithKey ::
  forall contained contained'.
  (forall key. PathTemplate key -> contained key -> contained' key) ->
  RoutingMap contained ->
  RoutingMap contained'
mapRoutingMapWithKey f = runIdentity . traverseRoutingMapWithKey (\k -> Identity . f k)

foldrRouteMatchesReversed :: [Text] -> (FoundRoute contained -> b -> b) -> b -> RoutingMap contained -> b
foldrRouteMatchesReversed paths f d (RouteTrunk t) =
  foldrRouteLeafMatches RPNil paths f d t

foldrRouteMatches :: [Text] -> (FoundRoute contained -> b -> b) -> b -> RoutingMap contained -> b
foldrRouteMatches = foldrRouteMatchesReversed . censorEmpty . reverse
  where
    censorEmpty (x : xs)
      | Text.null x = xs
    censorEmpty r = r

foldrRouteMatchesForFusion :: [Text] -> (FoundRoute contained -> b -> b) -> b -> RoutingMap contained -> b
foldrRouteMatchesForFusion = foldrRouteMatches

unionRoutingMapWith ::
  (forall v. contained v -> contained v -> contained v) ->
  RoutingMap contained ->
  RoutingMap contained ->
  RoutingMap contained
unionRoutingMapWith f (RouteTrunk l) (RouteTrunk r) =
  RouteTrunk $ unionRoutingMapLeafWith f l r

-- | Get a list of all routes that match the paths.
--
-- This list is lazy, and priorities matching static components over matching captures.
--
-- Probably subject to list fusion? IDK rewrite rules scare me.
allRouteMatches :: [Text] -> RoutingMap contained -> [FoundRoute contained]
allRouteMatches paths = foldrRouteMatches paths (:) []

-- | Get the best route match.
--
-- Subject to list fusion.
bestRouteMatch :: [Text] -> RoutingMap contained -> Maybe (FoundRoute contained)
bestRouteMatch paths = foldrRouteMatches paths f Nothing
  where
    f a _ = Just a

runRoutingMap :: RoutingMap contained -> [Text] -> Maybe (FoundRoute contained)
runRoutingMap = flip bestRouteMatch

{-# NOINLINE [0] foldrRouteMatchesForFusion #-}

{-# NOINLINE [0] allRouteMatches #-}

{-# RULES "routingMap.allRouteMatches" [~1] forall paths m. allRouteMatches paths m = build (\c n -> foldrRouteMatchesForFusion paths c n m) #-}

{-# RULES "routingMap.allRouteMatchesBack" [1] forall paths m. foldrRouteMatchesForFusion paths (:) [] m = allRouteMatches paths m #-}

{-# RULES "routingMap.singletonInsert" [~1] forall k v m. singletonRoutingMap k v <> m = insertRoutingMap k v m #-}
