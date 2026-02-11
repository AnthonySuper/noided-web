{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Effect.Translate
  ( HasTranslations,
    withMessagesFromQueryParams,
    withMessagesFromQueryParams',
  )
where

import Data.Maybe
import Data.Text
import Effectful
import Noided.Form
import Noided.Translate
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Internal.Effect.SomeRequest
import Noided.Web.Internal.Effect.Translate
import Optics.Core

withMessagesFromQueryParams' ::
  ( GetQueryParams :> es,
    HasTranslations :> es
  ) =>
  (FormSubmission UrlEncoded -> Maybe Text) ->
  Text ->
  Eff (FetchMessagesE : es) b ->
  Eff es b
withMessagesFromQueryParams' getTranslateParam defaultLanguage act = do
  lang <- fromMaybe defaultLanguage . getTranslateParam <$> getQueryParams
  translations <- readTranslations
  let messages = translationsForLocale lang translations
  runFetchMessagesE messages act

withMessagesFromQueryParams ::
  ( GetQueryParams :> es,
    HasTranslations :> es
  ) =>
  Text ->
  Eff (FetchMessagesE : es) b ->
  Eff es b
withMessagesFromQueryParams = withMessagesFromQueryParams' (preview $ _SubmissionObject % at "_lang" % _Just % _SubmissionValue % _TextValue)
