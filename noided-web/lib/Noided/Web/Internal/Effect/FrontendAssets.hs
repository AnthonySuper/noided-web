{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Web.Internal.Effect.FrontendAssets where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Noided.Web.Internal.Type.FrontendAssets

-- | Effect for retrieving frontend asset links (JS/CSS) for a given entry point.
data FrontendAssets :: Effect where
  GetAssetLinks :: Text -> FrontendAssets m AssetLinks

type instance DispatchOf FrontendAssets = Dynamic

-- | Retrieve asset links for an entry point (e.g., "static/main.ts").
getAssetLinks :: (FrontendAssets :> es) => Text -> Eff es AssetLinks
getAssetLinks = send . GetAssetLinks

-- | Run FrontendAssets in development mode, pointing to a Vite dev server.
runFrontendAssetsDev ::
  -- | Vite dev server base URL (e.g., "http://localhost:5173")
  Text ->
  Eff (FrontendAssets : es) a ->
  Eff es a
runFrontendAssetsDev baseUrl = interpret $ \_ -> \case
  GetAssetLinks entryPoint ->
    return
      AssetLinks
        { jsFiles = [baseUrl <> "/@vite/client", baseUrl <> "/" <> entryPoint],
          cssFiles = []
        }

-- | Run FrontendAssets in production mode using a parsed manifest.
runFrontendAssetsProd ::
  -- | Prefix for asset URLs (e.g., "/dist/")
  Text ->
  ViteManifest ->
  Eff (FrontendAssets : es) a ->
  Eff es a
runFrontendAssetsProd prefix manifest = interpret $ \_ -> \case
  GetAssetLinks entryPoint ->
    case Map.lookup entryPoint manifest.getManifest of
      Nothing -> return $ AssetLinks [] []
      Just entry ->
        return
          AssetLinks
            { jsFiles = [prefix <> entry.file],
              cssFiles = map (prefix <>) (fromMaybe [] entry.css)
            }

-- | Helper to load a Vite manifest from a file path.
loadViteManifest :: (IOE :> es) => FilePath -> Eff es (Either String ViteManifest)
loadViteManifest path = do
  liftIO (eitherDecodeFileStrict path)
