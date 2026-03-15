module OptBeer.Type.OrganizationIdent where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import OptBeer.DB.Ids.OrganizationId
import Web.HttpApiData

-- | An identifier for an organization, which can be either a numeric ID or a name.
data OrganizationIdent
  = OrganizationById OrganizationId
  | OrganizationByName Text
  deriving (Show, Read, Eq, Ord, Generic)

instance FromHttpApiData OrganizationIdent where
  parseUrlPiece t =
    if T.all isDigit t && not (T.null t)
      then case (parseUrlPiece t :: Either Text OrganizationId) of
        Left err -> Left err
        Right rid -> Right $ OrganizationById rid
      else Right $ OrganizationByName t

instance ToHttpApiData OrganizationIdent where
  toUrlPiece (OrganizationById rid) = toUrlPiece rid
  toUrlPiece (OrganizationByName name) = name
