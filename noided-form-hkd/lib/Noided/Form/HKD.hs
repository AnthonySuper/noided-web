module Noided.Form.HKD
  ( HKDForm (..),

    -- ** Deriving helpers
    GHKDFormLenses,
    ghkdFormLenses,
    GHKDFormLabels,
    ghkdFormLabels,
    GHKDFormHasErrors,
    ghkdFormHasErrors,

    -- * Re-Exported Types
    module Noided.Form.HKD.Type,
  )
where

import Noided.Form.HKD.Internal.Class
import Noided.Form.HKD.Type
