module Noided.Web.Html
  ( -- * Element ids
    DomId (..),
    showingDomId,
    idFromDomId,

    -- * Help rendering a form
    FormRendererT,
    FieldRendererT,
    runFormT,
    renderFormT,

    -- ** Building renderers
    formField,
    subformField,
    listField,

    -- *** Mapping/Transforming Renderers
    wrapField,
    fieldWrapModelName,
    fieldWrapAddToId,
    fieldModelName,
    fieldAddToId,

    -- *** Rendering Components of a Field
    renderBaseErrors,
    renderFieldErrors,
    renderInputTag,
    renderInputTag',
    renderTextareaTag,
    renderTextareaTag',
    renderLabelTag,
    renderFieldName,

    -- **** Attributes for components of a field
    fieldId,
    fieldName,
    fieldHasError,
    fieldBaseErrors,
    inputFieldValue,
    inputValueText,
    inputValueAttribute,
    inputAttributes',
    inputAttributes,
    labelAttributes,

    -- * Translating while rendering
    renderTranslated,
    renderErrorTranslatedWithPrefixes,
    renderErrorTranslated,

    -- * Utility classes for rendering

    -- ** Fetch translation messages
    FetchMessages (..),

    -- *** With Effectful
    FetchMessagesE (..),
    runFetchMessagesE,

    -- ** Fetch HTML formatters
    FetchHtmlFormatters (..),

    -- *** With Effectful
    FetchHtmlFormattersE (..),
    runFetchHtmlFormattersE,

    -- * HTML Formatters
    HtmlFormatter (..),
    HtmlFormatters,
    useHtmlFormatter,
    defaultFormatters,

    -- * Translation Monad Transformer
    TranslationT (..),
    TranslationEnv (..),
    translationEnvFromEnv,
    translateFromEnv,
    unwrapTranslationT,
  )
where

import Noided.Web.Html.Internal.Class.DomId
import Noided.Web.Html.Internal.Class.FetchHtmlFormatters
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Html.Internal.FormRenderer
import Noided.Web.Html.Internal.Translate
import Noided.Web.Html.Internal.Type.FormRendererT
import Noided.Web.Html.Internal.Type.HtmlFormatter
import Noided.Web.Html.Internal.Type.TranslationT
