module Noided.Web.Effect.WriteHeader
  ( WriteHeader,
    writeHeader,

    -- ** Interpreters
    runWriteHeaderMap,
    runIgnoringWrittenHeaders,
  )
where

import Noided.Web.Internal.Effect.WriteHeader
