{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Noided.Web.Internal.Type.FrontendAssets where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics

-- | Result of looking up an asset entry point.
data AssetLinks = AssetLinks
  { jsFiles :: [Text],
    cssFiles :: [Text]
  }
  deriving (Show, Eq, Generic)

-- | Structure of a Vite manifest entry.
data ViteManifestEntry = ViteManifestEntry
  { file :: !Text,
    css :: !(Maybe [Text])
  }
  deriving (Show, Eq, Generic)

instance FromJSON ViteManifestEntry

-- | Structure of the entire Vite manifest.
newtype ViteManifest = ViteManifest {getManifest :: Map.Map Text ViteManifestEntry}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON)
