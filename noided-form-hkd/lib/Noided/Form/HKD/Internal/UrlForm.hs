{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Form.HKD.Internal.UrlForm where

import Data.HKD
import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD.Internal.Class
import Noided.Form.HKD.Internal.Type.FormInput
import Noided.Form.HKD.Internal.Type.FormRenderer
import Noided.Form.HKD.Internal.Type.HKDFieldType
import Noided.Form.HKD.Internal.Type.WriteUrlEncoded
import Noided.Form.Types
import Web.HttpApiData

-- | Type of forms that can be rendered as query parameters in URLs.
class (HKDForm form, FZip form) => UrlForm form where
  -- | Get a URL-encoded-renderer for this form.
  --
  -- You should basically always let this be auto-derived.
  hkdFormUrlRenderer :: form (FormRenderer WriteUrlEncoded)
  default hkdFormUrlRenderer ::
    ( Generic (form (FormRenderer WriteUrlEncoded)),
      GUrlForm (Rep (form (FormRenderer WriteUrlEncoded)))
    ) =>
    form (FormRenderer WriteUrlEncoded)
  hkdFormUrlRenderer = ghkdFormUrlRenderer

fieldInputToText :: (ToHttpApiData a) => FieldInput a -> Text
fieldInputToText = \case
  NotPresent -> ""
  FromTyped t -> toQueryParam t
  FromForm (TextValue a) -> a
  FromForm (FileValue _) -> ""

-- | Generic deriving for 'hkdFormUrlRenderer'.
-- Will replace any file inputs with blank values.
-- Uses 'fieldInputToText' for its work.
class GUrlForm rep where
  genericHkdFormUrlRenderer :: rep ()

instance (GUrlForm l, GUrlForm r) => GUrlForm (l :*: r) where
  genericHkdFormUrlRenderer = genericHkdFormUrlRenderer :*: genericHkdFormUrlRenderer

instance (GUrlForm i) => GUrlForm (M1 tag md i) where
  genericHkdFormUrlRenderer = M1 genericHkdFormUrlRenderer

instance (HasUrlRenderer field) => GUrlForm (K1 i (FormRenderer WriteUrlEncoded field)) where
  genericHkdFormUrlRenderer = K1 hasUrlRenderer

instance GUrlForm U1 where
  genericHkdFormUrlRenderer = U1

class HasUrlRenderer field where
  hasUrlRenderer :: FormRenderer WriteUrlEncoded field

instance (ToHttpApiData t) => HasUrlRenderer (InputField t) where
  hasUrlRenderer = InputRenderer $ \ctx -> tellKeyValue ctx.key (fieldInputToText ctx.input.val)

instance (UrlForm subform) => HasUrlRenderer (SubformField subform) where
  hasUrlRenderer = SubformRenderer hkdFormUrlRenderer

instance (HasUrlRenderer inner) => HasUrlRenderer (ListField inner) where
  hasUrlRenderer = ListRenderer hasUrlRenderer

ghkdFormUrlRenderer ::
  ( Generic (form (FormRenderer WriteUrlEncoded)),
    GUrlForm (Rep (form (FormRenderer WriteUrlEncoded)))
  ) =>
  form (FormRenderer WriteUrlEncoded)
ghkdFormUrlRenderer = to genericHkdFormUrlRenderer
