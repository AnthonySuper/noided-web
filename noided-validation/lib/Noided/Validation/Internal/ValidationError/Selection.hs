{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Validation.Internal.ValidationError.Selection where

import Data.Text (Text, intercalate, pack)
import Data.Typeable
import GHC.Generics
import GHC.IsList (fromList)
import Noided.Translate
import Noided.Validation.Internal.ValidationError

-- | Error when a value is not one of the allowed options.
data InvalidSelection a = InvalidSelection
  { allowed :: [a],
    actual :: a
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (Typeable a, Show a, Ord a, AsTranslateParam a) => ValidationError (InvalidSelection a) where
  validationErrorTranslateParams (InvalidSelection allowed actual) =
    fromList
      [ ("allowed", ParamFragment $ intercalate ", " (map (paramToText . asTranslateParam) allowed)),
        ("actual", asTranslateParam actual)
      ]

-- | Error when a value is one of the forbidden options.
data ForbiddenSelection a = ForbiddenSelection
  { forbidden :: [a],
    actual :: a
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (Typeable a, Show a, Ord a, AsTranslateParam a) => ValidationError (ForbiddenSelection a) where
  validationErrorTranslateParams (ForbiddenSelection forbidden actual) =
    fromList
      [ ("forbidden", ParamFragment $ intercalate ", " (map (paramToText . asTranslateParam) forbidden)),
        ("actual", asTranslateParam actual)
      ]

paramToText :: TranslateParam -> Text
paramToText (ParamFragment t) = t
paramToText (ParamInt i) = pack (show i)
paramToText (ParamFloat f) = pack (show f)
