module OptBeer.DB.Type.PasswordDigest where

import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Define

newtype PasswordDigest = MkPasswordDigest {getPasswordDigest :: Text}
  deriving (Eq, Ord, Generic)

instance Show PasswordDigest where
  show _ = "PasswordDigest <CENSORED>"

instance PGType PasswordDigest where
  pgTypeName = pgTypeNameNewtype @Text

instance AsBindParam PasswordDigest where
  bindParamEncoder = bindParamEncoderNewtype @Text
  inspectBindParam = const "<CENSORED>"

instance AsHaskellValue PasswordDigest where
  type HaskellTypeOf PasswordDigest = PasswordDigest
  decodeHaskellValue = decodeNewtypeWrapper @Text
