module Noided.Server.Internal.Serve where

import Data.Maybe
import Network.Wai qualified as Wai
import Noided.Pathname
import Noided.Server.Internal.ParseRequest
import Noided.Server.Internal.Type.Action
import Noided.Server.Internal.Type.Server
import Noided.Server.Internal.Type.Verb
import Optics.Core

toWaiApplication ::
  Server IO Wai.Response -> Wai.Application
toWaiApplication (MkServer aa nfa) = \req cb ->
  fromMaybe (nfa req >>= cb) $ do
    RouteMatched params contained <- firstRouterMatch (Wai.pathInfo req) sharedRoutes
    verb <- parseVerbFromMethod (Wai.requestMethod req)
    Act act <- contained ^. at verb
    Just $ (withParsedRequest params req act) >>= cb
  where
    sharedRoutes = someActionsToRouter aa
