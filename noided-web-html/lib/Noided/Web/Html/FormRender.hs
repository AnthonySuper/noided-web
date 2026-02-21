module Noided.Web.Html.FormRender
  ( -- * Rendering forms
    renderFormT,
    runFormT,
    FormRendererT,
    FieldRendererT,

    -- * Building renderers
    formField,
    subformField,
    listField,

    -- * Rendering helpers
    renderFieldName,
    renderLabelTag,
    renderInputTag,
    renderInputTag',
    renderTextareaTag,
    renderTextareaTag',
    renderFieldErrors,
    renderBaseErrors,

    -- * Customizing renderers
    fieldWrapModelName,
    fieldWrapAddToId,
    fieldModelName,
    fieldAddToId,
    wrapField,

    -- * Context and Attributes
    fieldId,
    fieldName,
    inputAttributes,
    inputAttributes',
    labelAttributes,
    fieldHasError,
    fieldBaseErrors,
    inputFieldValue,
    inputValueText,
    inputValueAttribute,
  )
where

import Noided.Web.Html.Internal.FormRenderer
import Noided.Web.Html.Internal.Type.FormRendererT
