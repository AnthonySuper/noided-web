module OptBeer.ValidationError.BadUnitCategory where

import GHC.Generics
import Noided.Validation
import OptBeer.DB.Type.UnitCategory

data BadUnitCategory = BadUnitCategory
  { expected :: UnitCategory
  }
  deriving (Show, Eq, Ord, Generic)

instance ValidationError BadUnitCategory
