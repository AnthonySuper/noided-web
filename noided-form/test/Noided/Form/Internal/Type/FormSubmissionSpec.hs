{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Form.Internal.Type.FormSubmissionSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Noided.Form.Internal.Type.FormCanonicalKey
import Noided.Form.Internal.Type.FormContentType
import Noided.Form.Internal.Type.FormSubmission
import Noided.Form.Internal.Type.UploadedFile
import Optics.Core (itoListOf)
import Test.Hspec

spec :: Spec
spec = do
  describe "ixFormValues" $ do
    it "correctly traverses a nested submission and generates canonical keys" $ do
      let submission :: FormSubmission 'UrlEncoded
          submission =
            SubmissionObject $
              Map.fromList
                [ ( "user",
                    SubmissionObject $
                      Map.fromList
                        [ ("name", SubmissionValue (TextValue "Alice")),
                          ( "emails",
                            SubmissionArray $
                              Seq.fromList
                                [ SubmissionValue (TextValue "alice@example.com"),
                                  SubmissionValue (TextValue "alice@work.com")
                                ]
                          )
                        ]
                  )
                ]

          -- Expected keys and values
          expected :: [(FormCanonicalKey, FormValue 'UrlEncoded)]
          expected =
            [ ( MkFormCanonicalKey $
                  Seq.fromList
                    [ CanonicalObjectPiece "user",
                      CanonicalObjectPiece "emails",
                      CanonicalArrayPiece 0
                    ],
                TextValue "alice@example.com"
              ),
              ( MkFormCanonicalKey $
                  Seq.fromList
                    [ CanonicalObjectPiece "user",
                      CanonicalObjectPiece "emails",
                      CanonicalArrayPiece 1
                    ],
                TextValue "alice@work.com"
              ),
              ( MkFormCanonicalKey $
                  Seq.fromList
                    [ CanonicalObjectPiece "user",
                      CanonicalObjectPiece "name"
                    ],
                TextValue "Alice"
              )
            ]

          result :: [(FormCanonicalKey, FormValue 'UrlEncoded)]
          result = itoListOf ixFormValues submission

      result `shouldMatchList` expected

  describe "urlSubmissionToMultipartSubmission" $ do
    it "converts losslessy" $ do
      let input = SubmissionValue (TextValue "foo") :: FormSubmission UrlEncoded
      let output = urlSubmissionToMultipartSubmission input
      output `shouldBe` SubmissionValue (TextValue "foo")

  describe "multipartSubmissionToUrlSubmission" $ do
    it "removes top-level files" $ do
      let input = SubmissionValue (FileValue (MkUploadedFile "text/plain" "foo.txt" "/tmp/foo"))
      multipartSubmissionToUrlSubmission input `shouldBe` SubmissionEmpty

    it "removes files from arrays" $ do
      let file = SubmissionValue (FileValue (MkUploadedFile "text/plain" "foo.txt" "/tmp/foo"))
          text = SubmissionValue (TextValue "bar")
          input = SubmissionArray (Seq.fromList [file, text])
      multipartSubmissionToUrlSubmission input `shouldBe` SubmissionArray (Seq.fromList [SubmissionValue (TextValue "bar")])

    it "removes files from objects" $ do
      let file = SubmissionValue (FileValue (MkUploadedFile "text/plain" "foo.txt" "/tmp/foo"))
          text = SubmissionValue (TextValue "bar")
          input = SubmissionObject (Map.fromList [("f", file), ("t", text)])
      multipartSubmissionToUrlSubmission input `shouldBe` SubmissionObject (Map.fromList [("t", SubmissionValue (TextValue "bar"))])
