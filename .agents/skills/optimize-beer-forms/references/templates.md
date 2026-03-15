# Standard Form Templates

Use these as the starting point for any new form implementation in `optimize-beer`.

## 1. Type (`lib/OptBeer/Form/Type/MyForm.hs`)

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

module OptBeer.Form.Type.MyForm where

import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import Noided.Form.HKD.TH (defineHKDForm)

data MyFormF wrapper
  = MyForm
  { field1 :: wrapper (InputField Text),
    field2 :: wrapper (InputField Int)
  }
  deriving (Generic)

$(defineHKDForm ''MyFormF)

deriving instance (Show (wrapper (InputField Text)), Show (wrapper (InputField Int))) => Show (MyFormF wrapper)
```

## 2. Validation (`lib/OptBeer/Form/Validate/MyForm.hs`)

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Validate.MyForm where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Noided.Form.HKD
import Noided.Sql
import Noided.Validation
import OptBeer.Form.Type.MyForm

myFormValidator :: (Monad m) => FormValidator m (SubformField MyFormF)
myFormValidator = validateSubform $
  MyForm
    { field1 = validateInput return,
      field2 = validateInput checkRange
    }

checkRange :: Int -> ValidatorT m Int
checkRange val = do
  when (val < 0) $ failNonfatal MyRangeError
  return val
```

## 3. Renderer (`lib/OptBeer/Form/Render/MyForm.hs`)

```haskell
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Form.Render.MyForm where

import Lucid
import Noided.Form.HKD
import Noided.Web.Html.FormRender
import OptBeer.Form.Type.MyForm

myFormRenderer :: (FetchMessages m, FetchHtmlFormatters m) => HtmlFormRendererT m (SubformField MyFormF)
myFormRenderer =
  fieldWrapModelName "MyForm" $
    subformField myFormRendererT

myFormRendererT :: (FetchMessages m, FetchHtmlFormatters m) => MyFormF (FormRenderer (HtmlFormT m))
myFormRendererT =
  MyForm
    { field1 = textField,
      field2 = numberField
    }
  where
    textField = formField $ fieldWrapper $ renderInputTag [class_ "form-field-input", type_ "text"]
    numberField = formField $ fieldWrapper $ renderInputTag [class_ "form-field-input", type_ "number"]

    fieldWrapper inputAct = div_ [class_ "form-field-wrapper"] $ do
      renderFieldErrors (ul_ [class_ "form-field-errors"] . li_ [class_ "form-field-error"])
      renderLabelTag [class_ "form-field-label"]
      inputAct
```

## 4. Renderer Spec (`test/OptBeer/Form/Render/MyFormSpec.hs`)

```haskell
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Form.Render.MyFormSpec (spec) where

import Data.Functor.Identity (Identity)
import Lucid.Base (HtmlT)
import Noided.Form.HKD
import Noided.Translate
import Noided.Web.Html.FormRender
import Noided.Web.Html (TranslationT)
import OptBeer.Form.Render.MyForm
import OptBeer.Form.Render.SpecHelper
import Test.Hspec

spec :: SpecWith Translations
spec = describe "myFormRenderer" $ do
  withTranslationsInLocale "en" $ do
    it "renders without bad translations" $ \runner -> do
      let input = hkdFormEmpty
          errs = mempty
          renderAct :: HtmlT (TranslationT Identity) ()
          renderAct = renderFormT myFormRenderer input errs
          soup = runTranslationToSoup runner renderAct
      assertHasNoBadTranslations soup
```
