module Noided.Form.HKD.Internal.Type.FormRenderer where

import Data.HKD
import Data.Kind
import GHC.Generics
import Noided.Form
import Noided.Form.HKD.Internal.Type.FormErrors
import Noided.Form.HKD.Internal.Type.FormInput
import Noided.Form.HKD.Internal.Type.HKDFieldType

-- | Rendering context provided to each field renderer.
data RenderingContext field
  = RenderContext
  { -- | The user-inputted value
    input :: !(FormInput field),
    -- | Any errors which may be present
    errors :: !(FormErrors field),
    -- | The canonical path to this field.
    key :: !FormCanonicalKey
  }
  deriving (Generic)

-- | Type of *renderers* of forms.
type FormRenderer :: (Type -> Type) -> HKDFieldType -> Type
data FormRenderer m field where
  AroundRendering ::
    (forall a. RenderingContext field -> m a -> m a) ->
    FormRenderer m field ->
    FormRenderer m field
  InputRenderer ::
    (RenderingContext (InputField field) -> m ()) ->
    FormRenderer m (InputField field)
  SubformRenderer ::
    (FTraversable subform, FZip subform, Monoid (subform FormErrors)) =>
    subform (FormRenderer m) ->
    FormRenderer m (SubformField subform)
  ListRenderer ::
    FormRenderer m field ->
    FormRenderer m (ListField field)
