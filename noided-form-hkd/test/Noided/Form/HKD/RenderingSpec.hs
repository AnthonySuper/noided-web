{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Form.HKD.RenderingSpec (spec) where

import Control.Monad.Writer.Strict
import Data.Foldable (toList)
import Data.HKD
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Noided.Form
import Noided.Form.HKD
import Noided.Validation
import Test.Hspec

-- Forms
data Address f = Address
  { street :: f (InputField Text),
    city :: f (InputField Text)
  }
  deriving (Generic)

deriving instance (Show (f (InputField Text))) => Show (Address f)

instance FFunctor Address where ffmap = ffmapDefault

instance FFoldable Address where ffoldMap = ffoldMapDefault

instance FTraversable Address where ftraverse = gftraverse

instance FZip Address where fzipWith = gfzipWith

instance FRepeat Address where frepeat = gfrepeat

deriving via (Generically (Address FormErrors)) instance Semigroup (Address FormErrors)

deriving via (Generically (Address FormErrors)) instance Monoid (Address FormErrors)

instance HKDForm Address

data User f = User
  { name :: f (InputField Text),
    address :: f (SubformField Address),
    tags :: f (ListField (InputField Text))
  }
  deriving (Generic)

deriving instance (Show (f (InputField Text)), Show (f (SubformField Address)), Show (f (ListField (InputField Text)))) => Show (User f)

instance FFunctor User where ffmap = ffmapDefault

instance FFoldable User where ffoldMap = ffoldMapDefault

instance FTraversable User where ftraverse = gftraverse

instance FZip User where fzipWith = gfzipWith

instance FRepeat User where frepeat = gfrepeat

deriving via (Generically (User FormErrors)) instance Semigroup (User FormErrors)

deriving via (Generically (User FormErrors)) instance Monoid (User FormErrors)

instance HKDForm User

-- Renderer implementation
type Log = [Text]

type RenderM = Writer Log

logRender :: Text -> RenderM ()
logRender msg = tell [msg]

-- A generic text input renderer
textRenderer :: FormRenderer RenderM (InputField Text)
textRenderer = renderInput $ \ctx -> do
  let keyStr = T.intercalate "." $ map showPiece (toList $ unwrapCanonicalKey ctx.key)
  let valStr = case ctx.input.val of
        FromTyped t -> t
        FromForm _ -> "FORM_VAL"
        NotPresent -> "MISSING"
  let errStr = if ctx.errors.innerErrors `hasError` Blank then "ERR:Blank" else "OK"
  logRender $ "Input: " <> keyStr <> " = " <> valStr <> " (" <> errStr <> ")"
  where
    showPiece (CanonicalObjectPiece t) = t
    showPiece (CanonicalArrayPiece i) = T.pack (show i)
    unwrapCanonicalKey (MkFormCanonicalKey pieces) = pieces

-- Address Renderer
addressRenderer :: FormRenderer RenderM (SubformField Address)
addressRenderer =
  renderSubform $
    Address
      { street = textRenderer,
        city = textRenderer
      }

-- User Renderer
userRenderer :: FormRenderer RenderM (SubformField User)
userRenderer =
  renderSubform $
    User
      { name = textRenderer,
        address = addressRenderer,
        tags = renderList textRenderer
      }

spec :: Spec
spec = describe "HKD Form Rendering" $ do
  it "renders fields with correct keys and values" $ do
    let input =
          User
            { name = InputInput $ FromTyped "Alice",
              address =
                SubformInput $
                  Address
                    { street = InputInput $ FromTyped "Main St",
                      city = InputInput $ FromTyped "" -- Blank
                    },
              tags = ListInput $ Seq.fromList [InputInput $ FromTyped "tag1", InputInput $ FromTyped "tag2"]
            }

    let errors =
          User
            { name = mempty,
              address = subformErrors mempty $ Address {street = mempty, city = inputErrors (singletonError Blank)},
              tags = mempty
            }

    let formErrors' = subformErrors mempty errors

    let (_, logs) = runWriter $ renderForm userRenderer input formErrors'

    logs `shouldContain` ["Input: name = Alice (OK)"]
    logs `shouldContain` ["Input: address.street = Main St (OK)"]
    logs `shouldContain` ["Input: address.city =  (ERR:Blank)"]
    logs `shouldContain` ["Input: tags.0 = tag1 (OK)"]
    logs `shouldContain` ["Input: tags.1 = tag2 (OK)"]
