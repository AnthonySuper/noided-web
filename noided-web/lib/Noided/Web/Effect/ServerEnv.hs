module Noided.Web.Effect.ServerEnv
  ( GetServerEnv (..),
    getServerEnv,
    runServerEnv,

    -- * Server Environment Type
    ServerEnv (..),
    ServerEnvSing (..),
    KnownServerEnv (..),
    withServerEnv,
  )
where

import Noided.Web.Internal.Effect.ServerEnv
import Noided.Web.Internal.Type.ServerEnv
