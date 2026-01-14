{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Form.Internal.Type.FormSubmissionSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Noided.Form.Internal.Type.FormCanonicalKey
import Noided.Form.Internal.Type.FormContentType
import Noided.Form.Internal.Type.FormSubmission
import Optics.Core (itoListOf)
import Test.Hspec

spec :: Spec
spec = do
  describe "ixtraverseFormValues" $ do
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

      -- Actually, result is just a list.
      result `shouldMatchList` expected
