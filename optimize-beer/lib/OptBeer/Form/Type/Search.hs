{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Type.Search where

import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import Noided.Form.HKD.TH (defineHKDForm)
import OptBeer.Form.Type.Pagination

data SearchFormF wrapper
  = SearchForm
  { search :: wrapper (InputField Text),
    -- | Pagination part of search.
    -- Not rendered in the initial form, but used when rendering to URLs on
    -- next/back buttons.
    pagination :: wrapper (SubformField PaginationFormF)
  }
  deriving (Generic)

$(defineHKDForm ''SearchFormF)

deriving instance
  ( Show (wrapper (InputField Text)),
    Show (wrapper (SubformField PaginationFormF))
  ) =>
  Show (SearchFormF wrapper)
