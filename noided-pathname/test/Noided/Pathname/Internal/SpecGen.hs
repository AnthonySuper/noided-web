{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Pathname.Internal.SpecGen
  ( genSomePiece,
    genSomePath,
    genPathsWithSameCaptures,
    SomeRoute (..),
    genSomeRoute,
    invertComparison,
  )
where

import Data.Some.Newtype
import Data.Text (Text, pack, unpack)
import Noided.Pathname.Internal.PathTemplate
import Noided.Pathname.Internal.PieceTemplate
import Noided.Pathname.Internal.RouteParams
import Test.QuickCheck (Gen, arbitrary, choose, elements, frequency, oneof, vectorOf)

genCapturePiece :: Gen (Some PieceTemplate)
genCapturePiece =
  elements
    [ Some $ capPiece @Int,
      Some $ capPiece @Float,
      Some $ capPiece @String
    ]

-- | Plain lowercase text, which needs no escaping to appear in a URL.
genPlainText :: Gen Text
genPlainText = do
  len <- choose (0, 10)
  pack <$> vectorOf len genChar
  where
    genChar = do
      r <- choose (fromEnum 'a', fromEnum 'z')
      pure $ toEnum r

genStaticPiece :: Gen (Some PieceTemplate)
genStaticPiece = Some . StaticPiece <$> genPlainText

genSomePiece :: Gen (Some PieceTemplate)
genSomePiece = oneof [genStaticPiece, genCapturePiece]

genSomePath :: Gen (Some PathTemplate)
genSomePath = do
  len <- choose (0, 10)
  pieces <- vectorOf len genSomePiece
  pure $ buildSomePathTemplate pieces

-- | Generate @n@ templates that all capture the same list of types, but differ
-- in the static pieces woven between the captures.
--
-- Random templates almost never land in the same equivalence class, so the
-- properties over 'testEquality' -- which is meant to answer \"do these capture
-- the same types\" -- would be vacuous without this.
genPathsWithSameCaptures :: Int -> Gen [Some PathTemplate]
genPathsWithSameCaptures n = do
  capCount <- choose (0, 4)
  caps <- vectorOf capCount genCapturePiece
  vectorOf n (weaveStatics caps)
  where
    weaveStatics caps = do
      groups <- vectorOf (length caps + 1) genStaticGroup
      pure . buildSomePathTemplate $ weave groups caps
    genStaticGroup = do
      count <- choose (0, 2)
      vectorOf count genStaticPiece
    weave (g : gs) (c : cs) = g <> (c : weave gs cs)
    weave gs [] = concat gs
    weave [] cs = cs

buildSomePathTemplate :: (Foldable f) => f (Some PieceTemplate) -> Some PathTemplate
buildSomePathTemplate = foldr f (Some PathEnd)
  where
    f :: Some PieceTemplate -> Some PathTemplate -> Some PathTemplate
    f spiece spath =
      withSome spiece $ \piece ->
        withSome spath $ \path ->
          Some $ piece :/ path

-- | A path template together with a set of params that match it.
data SomeRoute where
  SomeRoute ::
    (Eq (RouteParams caps), Show (RouteParams caps)) =>
    PathTemplate caps ->
    RouteParams caps ->
    SomeRoute

instance Show SomeRoute where
  showsPrec d (SomeRoute template params) =
    showParen (d > 10) $
      showString "SomeRoute "
        . showsPrec 11 template
        . showString " "
        . showsPrec 11 params

-- | A capture, along with a value of the type it captures.
data SomeCaptureValue where
  SomeCaptureValue ::
    (KnownCapture a, Eq a, Show a) =>
    PieceTemplate (Just a) ->
    a ->
    SomeCaptureValue

data SomeRoutePiece
  = StaticRoutePiece Text
  | CaptureRoutePiece SomeCaptureValue

-- | Text that needs escaping to survive a trip through a URL.
--
-- Deliberately full of the characters that break an unescaped router: @\/@
-- would split one piece into two, @?@ and @#@ would end the path early, and
-- spaces and non-ASCII characters are not legal in a URL at all.
genEscapableText :: Gen Text
genEscapableText = do
  len <- choose (1, 8)
  pack <$> vectorOf len genEscapableChar
  where
    genEscapableChar =
      frequency
        [ (6, elements ['a' .. 'z']),
          (3, elements "/?#% +&=:@,.~-"),
          (1, elements "é日本語ß✓")
        ]

-- | Captures generated with values that round-trip through
-- 'Web.HttpApiData.toUrlPiece' and back, so a failed round-trip is the
-- library's fault and not the value's. That rules out 'Float'.
genSomeCaptureValue :: Gen SomeCaptureValue
genSomeCaptureValue =
  oneof
    [ SomeCaptureValue (capPiece @Int) <$> arbitrary,
      SomeCaptureValue (capPiece @Text) <$> genEscapableText,
      SomeCaptureValue (capPiece @String) . unpack <$> genEscapableText
    ]

-- | Static pieces are escapable too, and are non-empty: an empty static piece
-- generates an empty URL piece, which is not a piece at all.
genSomeRoutePiece :: Gen SomeRoutePiece
genSomeRoutePiece =
  oneof
    [ StaticRoutePiece <$> genEscapableText,
      CaptureRoutePiece <$> genSomeCaptureValue
    ]

genSomeRoute :: Gen SomeRoute
genSomeRoute = do
  len <- choose (0, 8)
  buildSomeRoute <$> vectorOf len genSomeRoutePiece

buildSomeRoute :: [SomeRoutePiece] -> SomeRoute
buildSomeRoute = foldr f (SomeRoute PathEnd RPNil)
  where
    f (StaticRoutePiece t) (SomeRoute template params) =
      SomeRoute (StaticPiece t :/ template) params
    f (CaptureRoutePiece (SomeCaptureValue cap v)) (SomeRoute template params) =
      SomeRoute (cap :/ template) (v :-$ params)

invertComparison :: Ordering -> Ordering
invertComparison EQ = EQ
invertComparison LT = GT
invertComparison GT = LT
