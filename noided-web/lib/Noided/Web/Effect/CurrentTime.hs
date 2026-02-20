module Noided.Web.Effect.CurrentTime
  ( CurrentTime,
    getCurrentTime,

    -- ** Interpreters
    runCurrentTime,
    runStaticTime,
  )
where

import Noided.Web.Internal.Effect.CurrentTime
