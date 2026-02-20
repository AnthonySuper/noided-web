module Noided.Web.Internal.Type.DBSettings where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import GHC.Generics
import Hasql.Connection.Settings

-- | Newtype wrapper for database settings, providing a @FromJSON@ instance.
--
-- If the parsed JSON value is a string, then this will be used as a connection string.
-- Otherwise, if it's an object, then each object key will be mapped as follows
-- and appended with the `Semigroup` instance for 'Hasql.Connection.Settings.Settings'.
--
--    - @host@ (a text value)
--    - @user@ (a text value)
--    - @password@ (a text value)
--    - @dbname@ (a text value)
--    - @applicationName@ (a text value)
--
-- None of these values are required when parsing JSON, but omitting some
-- /may/ cause connecting to fail.
--
-- Any other keys will be mapped with the @other@ function from Hasql.
newtype DBSettings = DBSettings {getHasqlSettings :: Settings}

instance FromJSON DBSettings where
  parseJSON = \case
    String t -> return $ DBSettings $ connectionString t
    Object o -> do
      let mapping =
            [ ("host", host),
              ("user", user),
              ("password", password),
              ("dbname", dbname),
              ("applicationName", applicationName)
            ]
          process (jsonKey, settingsFunc) =
            case KeyMap.lookup (Key.fromText jsonKey) o of
              Just (String val) -> Just $ settingsFunc val
              _ -> Nothing
          matchingPairs = mapMaybe process mapping
          otherPairs =
            [ other (Key.toText k) val
            | (k, String val) <- KeyMap.toList o,
              not (Key.toText k `elem` fmap fst mapping)
            ]
      case matchingPairs ++ otherPairs of
        [] -> return $ DBSettings $ connectionString ""
        (x : xs) -> return $ DBSettings $ sconcat (x :| xs)
    invalid -> prependFailure "parsing DBSettings failed, " (typeMismatch "String or Object" invalid)

data DBFileConfig
  = DBFileConfig
  { development :: Maybe DBSettings,
    test :: Maybe DBSettings
  }
  deriving (Generic)

instance FromJSON DBFileConfig where
  parseJSON =
    genericParseJSON $
      defaultOptions {omitNothingFields = True}
