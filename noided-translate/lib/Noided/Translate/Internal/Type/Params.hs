{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Noided.Translate.Internal.Type.Params where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.Text (Text, pack)
import GHC.Generics
import GHC.IsList
import GHC.TypeLits
import Noided.Translate.Internal.Type.Message (PluralizationForm (..))
import Optics.Core

data TranslateParam
  = ParamFragment Text
  | ParamInt Integer
  | ParamFloat Double
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (Generically TranslateParam)

paramToPluralizationForm :: TranslateParam -> Maybe PluralizationForm
paramToPluralizationForm = \case
  ParamFragment _ -> Nothing
  ParamInt i -> Just $ if i == 1 then One else Many
  ParamFloat i -> Just $ if i == 1 then One else Many

class AsTranslateParam t where
  asTranslateParam :: t -> TranslateParam

instance AsTranslateParam TranslateParam where
  asTranslateParam = id

instance AsTranslateParam Text where
  asTranslateParam = ParamFragment

instance AsTranslateParam Integer where
  asTranslateParam = ParamInt

instance AsTranslateParam Double where
  asTranslateParam = ParamFloat

shownParam :: TranslateParam -> Text
shownParam = \case
  ParamFragment f -> f
  ParamInt i -> pack $ show i
  ParamFloat d -> pack $ show d

newtype TranslateParams = TranslateParams {getTranslateParams :: Map.Map Text TranslateParam}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (Semigroup, Monoid, ToJSON, FromJSON) via (Map.Map Text TranslateParam)

instance IsList TranslateParams where
  type Item TranslateParams = (Text, TranslateParam)
  fromList = TranslateParams . fromList
  toList = toList . getTranslateParams

class GAsTranslateParams r where
  genericAsTranslateParams :: r () -> TranslateParams

instance
  (AsTranslateParam param, KnownSymbol s) =>
  GAsTranslateParams (S1 (MetaSel (Just s) i i' i'') (Rec0 param))
  where
  genericAsTranslateParams (M1 (K1 r)) = [(textVal, asTranslateParam r)]
    where
      textVal = pack $ symbolVal $ Proxy @s

instance
  (GAsTranslateParams lhs, GAsTranslateParams rhs) =>
  GAsTranslateParams (lhs :*: rhs)
  where
  genericAsTranslateParams (l :*: r) =
    genericAsTranslateParams l
      <> genericAsTranslateParams r

instance
  (GAsTranslateParams inner) =>
  GAsTranslateParams (D1 md inner)
  where
  genericAsTranslateParams (M1 r) =
    genericAsTranslateParams r

instance
  (GAsTranslateParams inner) =>
  GAsTranslateParams (C1 md inner)
  where
  genericAsTranslateParams (M1 r) =
    genericAsTranslateParams r

gasTranslateParams ::
  (Generic a, GAsTranslateParams (Rep a)) =>
  a ->
  TranslateParams
gasTranslateParams = genericAsTranslateParams . from

type instance Index TranslateParams = Text

type instance IxValue TranslateParams = TranslateParam

instance Ixed TranslateParams

instance At TranslateParams where
  at t = translateParamsMap % at t

translateParamsMap :: Iso' TranslateParams (Map.Map Text TranslateParam)
translateParamsMap = iso getTranslateParams TranslateParams
