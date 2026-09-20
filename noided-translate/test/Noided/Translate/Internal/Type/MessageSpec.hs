{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Translate.Internal.Type.MessageSpec (spec) where

import Noided.Translate (TranslateParam (..))
import Noided.Translate.Internal.Type.Message (Message (..), parseMessage, simplifySyn)
import Noided.Translate.SpecHelper
import Test.Hspec

shouldSimplifyTo :: [Message] -> [Message] -> Expectation
shouldSimplifyTo lhs rhs = simplifySyn lhs `shouldBe` rhs

-- | The one group that legitimately looks at the AST, because 'simplifySyn' is
-- a function over the AST rather than something a rendered message can show.
simplifySpec :: Spec
simplifySpec = describe "simplification" $ do
  it "can simplify two fragments" $
    [Fragment "foo", Fragment "bar"] `shouldSimplifyTo` [Fragment "foobar"]
  it "can simplify nested syns" $
    [Syn [Fragment "foo"]] `shouldSimplifyTo` [Fragment "foo"]
  it "can apply further simplification to nested syns" $
    [Fragment "foo", Syn [Fragment "bar"]] `shouldSimplifyTo` [Fragment "foobar"]

renderingSpec :: Spec
renderingSpec = describe "rendering" $ do
  it "renders a plain fragment unchanged" $
    "foo" `rendersTo` "foo"
  it "interpolates a variable" $
    rendersWith "foo $bar" [("bar", ParamFragment "baz")] "foo baz"
  it "renders a missing variable as its own name" $
    "foo $bar" `rendersTo` "foo $bar"
  it "renders a calculation" $
    rendersWith
      "foo {pluralize ( $bar ) { default { yeah } }}"
      [("bar", ParamInt 3)]
      "foo yeah "

-- | Messages that used to render as a silently truncated prefix, because
-- 'Data.Attoparsec.Text.parseOnly' is happy to leave input unconsumed.
truncationSpec :: Spec
truncationSpec = describe "leftover input" $ do
  it "rejects a trailing bare $ instead of dropping it" $
    shouldFailToParse "Cost: 5$"
  it "says something useful about a trailing bare $" $
    parseMessage "Cost: 5$"
      `shouldBe` Left "parse: unexpected input at character 7 of message \"Cost: 5$\": \"$\""
  it "rejects an unescaped brace instead of truncating the message" $
    shouldFailToParse "Save {50%}"
  it "rejects an unescaped closing brace" $
    shouldFailToParse "Save 50%}"
  it "rejects an unknown pluralization form" $
    shouldFailToParse "{pluralize ($n) { few { a few } default { none } }}"
  it "rejects a pluralize with no default clause" $
    shouldFailToParse "{pluralize ($n) { one { one } }}"
  it "rejects an unterminated calc" $
    shouldFailToParse "{pluralize ($n) { default { none } }"

escapeSpec :: Spec
escapeSpec = describe "escapes" $ do
  it "renders $$ as a literal $" $
    "Cost: 5$$" `rendersTo` "Cost: 5$"
  it "renders ${ as a literal {" $
    "Save ${50%" `rendersTo` "Save {50%"
  it "renders $} as a literal }" $
    "Save 50%$}" `rendersTo` "Save 50%}"
  it "renders a brace-wrapped literal" $
    "Save ${50%$}" `rendersTo` "Save {50%}"
  it "renders every escape alongside a real variable" $
    rendersWith "$$ ${ $} $count!" [("count", ParamInt 3)] "$ { } 3!"
  it "renders ${ inside a pluralize arm" $
    "{pluralize ($n) { default { ${none } }}" `rendersTo` "{none "
  -- The escaped brace is not structural, so the message still needs all three
  -- of the closing braces that end the arm, the arms block and the calculation.
  it "renders $} inside a pluralize arm" $
    "{pluralize ($n) { default { none$} }}}" `rendersTo` "none} "
  it "renders every escape alongside a real variable inside a pluralize arm" $
    rendersWith
      "{pluralize ($n) { default { $$ ${ $} $count! } }}"
      [("count", ParamInt 7)]
      "$ { } 7! "

defaultPositionSpec :: Spec
defaultPositionSpec = describe "the position of the default clause" $ do
  -- Every arm has to keep selecting correctly no matter where @default@ sits,
  -- so each case exercises all three: the singular, the plural, and the
  -- fallback for a value with no plural form of its own.
  let selectsEveryArm msg = do
        rendersWith msg [("n", ParamInt 1)] "one "
        rendersWith msg [("n", ParamInt 7)] "lots "
        rendersWith msg [("n", ParamFragment "several")] "none "
        msg `rendersTo` "none "
  it "selects every arm with default last" $
    selectsEveryArm "{pluralize ($n) { one { one } many { lots } default { none } }}"
  it "selects every arm with default in the middle" $
    selectsEveryArm "{pluralize ($n) { one { one } default { none } many { lots } }}"
  it "selects every arm with default first" $
    selectsEveryArm "{pluralize ($n) { default { none } one { one } many { lots } }}"
  it "uses the first default clause when there is more than one" $
    "{pluralize ($n) { default { none } default { other } }}" `rendersTo` "none "

-- | Messages taken verbatim from @optimize-beer/config/translations/en@, so
-- that a change to the parser cannot quietly invalidate a real translation
-- file (which 'Noided.Web.Internal.Effect.Translate.loadFile' would then drop
-- wholesale).
realWorldSpec :: Spec
realWorldSpec = describe "real translation strings" $ do
  it "renders errors.NotEnoughReqChars" $
    rendersWith
      "Missing required characters in category: $category (need at least $minAmount)."
      [("category", ParamFragment "symbols"), ("minAmount", ParamInt 2)]
      "Missing required characters in category: symbols (need at least 2)."
  it "renders form.password_policy.min_length in the singular" $
    rendersWith
      "{pluralize($count) { one { At least $count character long } default { At least $count characters long } }}"
      [("count", ParamInt 1)]
      "At least 1 character long "
  it "renders form.password_policy.min_length in the plural" $
    rendersWith
      "{pluralize($count) { one { At least $count character long } default { At least $count characters long } }}"
      [("count", ParamInt 8)]
      "At least 8 characters long "

spec :: Spec
spec = do
  simplifySpec
  renderingSpec
  truncationSpec
  escapeSpec
  defaultPositionSpec
  realWorldSpec
