module Noided.Server
  ( -- * Servers
    Server,
    makeServer,

    -- ** Serving
    toWaiApplication,

    -- ** Transforming
    mapServerResponse,
    hoistServerMonad,

    -- * Actions
    Action (..),
    mapActionResponse,
    transformAction,
    hoistActionMonad,

    -- ** Anonymous Actions
    SomeAction (..),
    aroundSomeAction,
    mapSomeActionResponseMonadic,
    hoistSomeActionMonad,

    -- * Requests
    Request (..),
  )
where

import Noided.Server.Internal.Serve
import Noided.Server.Internal.Type.Action
import Noided.Server.Internal.Type.Request
import Noided.Server.Internal.Type.Server
import Noided.Server.Internal.Type.Verb
