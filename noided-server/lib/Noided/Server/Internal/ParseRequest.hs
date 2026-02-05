{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Server.Internal.ParseRequest (withParsedRequest) where

import Control.Monad.Trans.Resource
import Data.Aeson (eitherDecode)
import Data.Bifunctor
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (toStrict)
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.HTTP.Types.URI
import Network.Wai qualified as Wai
import Network.Wai.Parse hiding (UrlEncoded)
import Noided.Form
import Noided.Form.Types
import Noided.Pathname
import Noided.Server.Internal.Type.Request
import Optics.Core

withParsedRequest ::
  RouteParams pathParams ->
  Wai.Request ->
  (Request pathParams -> IO a) ->
  IO a
withParsedRequest pp wr cb =
  withRequestBody wr $ \requestBody ->
    cb
      ( MkRequest
          pp
          requestBody
          (qsFromRequest wr)
          (Map.fromList $ Wai.requestHeaders wr)
          (Wai.remoteHost wr)
      )

qsFromRequest :: Wai.Request -> FormSubmission UrlEncoded
qsFromRequest = fromTextKeysAndValues . fmap (second (TextValue <$>)) . queryToQueryText . Wai.queryString

withRequestBody ::
  Wai.Request ->
  (RequestBody -> IO a) ->
  IO a
withRequestBody req cb = do
  let len = Wai.requestBodyLength req
  case len of
    Wai.KnownLength 0 -> cb NoBody
    _ -> do
      let mContentType = lookup "Content-Type" (Wai.requestHeaders req)
          parsedCT = fmap parseContentType mContentType
          cleanCT = fmap (first (BS8.map toLower)) parsedCT

      case cleanCT of
        Just ("application/json", _) -> do
          body <- Wai.strictRequestBody req
          case eitherDecode body of
            Right v -> cb (JSONBody v)
            Left err -> cb (MalformedBody (pack err))
        Just ("multipart/form-data", _) ->
          withParsedMultipartForm req (cb . FormBody . MultipartFormDataSubmission)
        Just ("application/x-www-form-urlencoded", _) ->
          withParsedUrlEncodedForm req (cb . FormBody . UrlEncodedSubmission)
        _ -> do
          let reader = do
                chunk <- Wai.getRequestBodyChunk req
                if BS8.null chunk
                  then return EndOfInput
                  else return (ActualChunk chunk)
          cb (UnknownBody $ ReqBodyUnknown parsedCT reader)

withParsedUrlEncodedForm ::
  Wai.Request ->
  (FormSubmission UrlEncoded -> IO a) ->
  IO a
withParsedUrlEncodedForm req cb = do
  body <- Wai.strictRequestBody req
  let query = parseQuery (toStrict body)
      inputs =
        [ (decodeUtf8Lenient k, TextValue . decodeUtf8Lenient <$> mv)
          | (k, mv) <- query
        ]
      form = fromTextKeysAndValues inputs
  cb form

requestBodyOptions :: ParseRequestBodyOptions
requestBodyOptions =
  setMaxRequestKeyLength 10000 defaultParseRequestBodyOptions

withParsedMultipartForm ::
  Wai.Request ->
  (FormSubmission MultipartFormData -> IO a) ->
  IO a
withParsedMultipartForm req cb = runResourceT $ withInternalState $ \internalState -> do
  (params, files) <- parseRequestBodyEx requestBodyOptions (tempFileBackEnd internalState) req
  let paramInputs =
        [ (decodeUtf8Lenient k, TextValue $ decodeUtf8Lenient v)
          | (k, v) <- params
        ]
      fileInputs =
        [ ( decodeUtf8Lenient k,
            FileValue
              ( MkUploadedFile
                  (decodeUtf8Lenient $ fileContentType fi)
                  (decodeUtf8Lenient $ fileName fi)
                  (fileContent fi)
              )
          )
          | (k, fi) <- files
        ]
      inputs = paramInputs ++ fileInputs
      form = fromTextKeysAndValues (inputs & mapped % _2 %~ Just)
  cb form
