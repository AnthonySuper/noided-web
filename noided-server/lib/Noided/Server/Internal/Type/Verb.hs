module Noided.Server.Internal.Type.Verb where

import GHC.Generics
import Network.HTTP.Types

-- | Type of an HTTP verb.
data Verb = GET | POST | PUT | PATCH | DELETE | OPTIONS
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

parseVerbFromMethod :: Method -> Maybe Verb
parseVerbFromMethod = error "TODO: implement me"
