module Noided.Web.Html.FormRender
  ( -- * Rendering forms
    renderFormT,
    runFormT,
    FormRendererT,
    FieldRendererT,
    HtmlFormT,
    HtmlFieldT,
    HtmlFormRendererT,

    -- * Building renderers
    formField,
    subformField,
    listField,

    -- * Rendering helpers
    renderFieldName,
    renderLabelTag,
    renderInputTag,
    renderInputTag',
    renderSelectTag,
    renderEnumSelectTag,
    renderEnumSelectTag',
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
    FetchMessages,
    FetchHtmlFormatters,
  )
where

import Noided.Web.Html.Internal.Class.FetchHtmlFormatters
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Html.Internal.FormRenderer
import Noided.Web.Html.Internal.Type.FormRendererT
