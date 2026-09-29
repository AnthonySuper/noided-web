{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Pathname.Internal.PathTemplate where

import Data.GADT.Compare
import Data.GADT.Show
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Type.Equality
import GHC.Generics hiding (prec)
import Network.HTTP.Types.URI (decodePathSegments, urlEncode)
import Noided.Pathname.Internal.PathCaptures
import Noided.Pathname.Internal.PieceTemplate
import Noided.Pathname.Internal.RouteParams
import Text.Read
import Type.Reflection
import Unsafe.Coerce (unsafeCoerce)
import Web.HttpApiData

type ConsPieceToCaps :: Maybe Type -> [Type] -> [Type]
type family ConsPieceToCaps piece caps where
  ConsPieceToCaps (Just piece) caps = piece ': caps
  ConsPieceToCaps Nothing caps = caps

infixr 5 :/

-- | A path template, which specifies the format of a URL.
data PathTemplate (args :: [Type]) where
  (:/) :: !(PieceTemplate piece) -> !(PathTemplate caps) -> PathTemplate (ConsPieceToCaps piece caps)
  PathEnd :: PathTemplate '[]

deriving instance Show (PathTemplate arg)

instance GShow PathTemplate where
  gshowsPrec = defaultGshowsPrec

instance Read (PathTemplate '[]) where
  readListPrec = readListPrecDefault
  readPrec = readEmpty +++ readCons
    where
      readCons = parens $ prec 5 $ do
        (item :: PieceTemplate Nothing) <- step readPrec
        Symbol ":/" <- lexP
        (r :: PathTemplate '[]) <- readPrec
        pure $ item :/ r

      readEmpty = do
        Ident "PathEnd" <- lexP
        pure PathEnd

instance (Read (PathTemplate rest), Read (PieceTemplate (Just a))) => Read (PathTemplate (a ': rest)) where
  readListPrec = readListPrecDefault
  readPrec = parens $ prec 5 $ do
    (h :: Either (PieceTemplate (Just a)) (PieceTemplate Nothing)) <-
      step $ (Left <$> readPrec) +++ (Right <$> readPrec)
    Symbol ":/" <- lexP
    case h of
      Left pt -> (pt :/) <$> readPrec @(PathTemplate rest)
      Right pt -> (pt :/) <$> readPrec @(PathTemplate (a ': rest))

instance Eq (PathTemplate params) where
  (==) = defaultEq

instance Ord (PathTemplate params) where
  compare = defaultCompare

pathNoCaptures :: PathTemplate a -> Maybe (a :~: '[])
pathNoCaptures PathEnd = Just Refl
pathNoCaptures (StaticPiece _ :/ r) = pathNoCaptures r
pathNoCaptures (CapPiece :/ _) = Nothing

instance TestEquality PathTemplate where
  testEquality PathEnd = \r -> do
    Refl <- pathNoCaptures r
    pure Refl
  testEquality (StaticPiece _ :/ rest) = \case
    h :/ rest' ->
      case h of
        StaticPiece _ -> testEquality rest rest'
        -- The right-hand side captures here and we do not, so drop our static
        -- piece and try again. This mirrors the 'CapPiece' case below, which
        -- drops a static piece on the right; without it 'testEquality' is not
        -- symmetric.
        CapPiece -> testEquality rest (h :/ rest')
    PathEnd -> testEquality rest PathEnd
  testEquality r@(c@CapPiece :/ rest) = \case
    h :/ rest' ->
      case h of
        StaticPiece _ -> testEquality r rest'
        c'@CapPiece -> do
          Refl <- testEquality c c'
          Refl <- testEquality rest rest'
          pure Refl
    PathEnd -> Nothing

instance GEq PathTemplate where
  geq PathEnd PathEnd = Just Refl
  geq PathEnd (_ :/ _) = Nothing
  geq (_ :/ _) PathEnd = Nothing
  geq (h :/ t) (h' :/ t') = do
    Refl <- geq h h'
    Refl <- geq t t'
    Just Refl

instance GCompare PathTemplate where
  gcompare PathEnd PathEnd = GEQ
  gcompare PathEnd (_ :/ _) = GLT
  gcompare (_ :/ _) PathEnd = GGT
  gcompare (h :/ t) (h' :/ t') =
    case gcompare h h' of
      GLT -> GLT
      GGT -> GGT
      GEQ -> case gcompare t t' of
        GLT -> GLT
        GGT -> GGT
        GEQ -> GEQ

pathTemplateToCapturesSing :: PathTemplate caps -> PathCaptureSing caps
pathTemplateToCapturesSing = \case
  PathEnd -> PCSNil
  (StaticPiece _ :/ r) -> pathTemplateToCapturesSing r
  (CapPiece @cap :/ r) ->
    typeRep @cap ::$ pathTemplateToCapturesSing r

-- | Internal function used to prove to the compiler that if you append an empty template
-- to any template, it does not change its capture parameters.
--
-- This function is rewritten with rewrite rules to produce a Refl out of thin air.
provePathTemplateAppendNothingDoesNothing :: PathTemplate caps -> AppendPathCaptures caps '[] :~: caps
provePathTemplateAppendNothingDoesNothing = proveAppendNothingDoesNothingSing . pathTemplateToCapturesSing
{-# NOINLINE [1] provePathTemplateAppendNothingDoesNothing #-}

{-# RULES
"removeProveAppendNothingDoesNothingProof" forall p.
  provePathTemplateAppendNothingDoesNothing p =
    p `seq` unsafeCoerce Refl
  #-}

provePathTemplateAppendIsAssociative ::
  forall a b c.
  PathTemplate a ->
  AppendPathCaptures (AppendPathCaptures a b) c
    :~: AppendPathCaptures a (AppendPathCaptures b c)
provePathTemplateAppendIsAssociative =
  proveAppendIsAssociativeSing @_ @b @c . pathTemplateToCapturesSing
{-# NOINLINE [1] provePathTemplateAppendIsAssociative #-}

{-# RULES
"removeProvePathTemplateAppendIsAssociativeProof" forall p.
  provePathTemplateAppendIsAssociative p =
    p `seq` unsafeCoerce Refl
  #-}

type AppendPathTemplate lhs rhs res = (AppendPathCaptures lhs rhs ~ res)

appendPathTemplate :: PathTemplate lhs -> PathTemplate rhs -> PathTemplate (AppendPathCaptures lhs rhs)
appendPathTemplate lhs rhs =
  case lhs of
    PathEnd -> rhs
    (h :/ t) ->
      case h of
        StaticPiece _ -> h :/ appendPathTemplate t rhs
        CapPiece -> h :/ appendPathTemplate t rhs

class UsePath args result | args -> result where
  usePathR :: Text -> PathTemplate args -> result

instance UsePath '[] Text where
  usePathR !t = \case
    (StaticPiece piece :/ rest) -> usePathR (t <> encodePathPiece piece <> "/") rest
    PathEnd -> t

instance (UsePath rest restResult) => UsePath (templateArg ': rest) (templateArg -> restResult) where
  usePathR !txt template arg =
    case template of
      (CapPiece :/ rest) -> usePathR (txt <> encodePathPiece (toUrlPiece arg) <> "/") rest
      (StaticPiece piece :/ rest) -> usePathR (txt <> encodePathPiece piece <> "/") rest arg

pathTemplateAppendStatic :: Text -> PathTemplate r -> PathTemplate r
pathTemplateAppendStatic txt = go
  where
    go :: PathTemplate caps -> PathTemplate caps
    go PathEnd = StaticPiece txt :/ PathEnd
    go (h :/ t) = h :/ go t

usePathTemplate :: (UsePath args result) => PathTemplate args -> result
usePathTemplate = usePathR "/"

-- | Percent-encode a single piece of a URL.
--
-- Every character that is not allowed unescaped in a path segment is escaped,
-- including @\/@, @?@, @#@ and spaces, and non-ASCII characters are escaped as
-- their UTF-8 bytes. 'Web.HttpApiData.toUrlPiece' deliberately does /not/ do
-- this, so every piece we put into a generated URL has to go through here, or
-- a capture containing a @\/@ would silently generate a URL for a different
-- route.
--
-- This is @http-types@\' @encodePathSegment@, specialised to 'Text'.
encodePathPiece :: Text -> Text
encodePathPiece = decodeUtf8With lenientDecode . urlEncode False . encodeUtf8

-- | Split a URL path into the percent-decoded pieces that 'matchPathTemplate'
-- and 'Noided.Pathname.firstRouterMatch' expect, undoing 'encodePathPiece' on
-- each one.
--
-- The empty piece in front of the leading @\/@ of an absolute path is dropped,
-- so the output can be handed straight to a matcher:
--
-- >>> splitPathPieces "/users/42"
-- ["users","42"]
--
-- A trailing @\/@, on the other hand, is kept, as a trailing empty piece:
--
-- >>> splitPathPieces "/users/42/"
-- ["users","42",""]
--
-- That is deliberate, and harmless: both matchers treat a single trailing empty
-- piece as the end of the path, so a URL with a trailing slash matches the same
-- templates as one without. It matters because 'usePathTemplate' and
-- 'usePathTemplateParams' always emit a trailing slash, and this function is
-- their inverse.
--
-- The root path is the one place the two collapse together: both @"\/"@ and
-- @""@ split to @[]@, which matches 'PathEnd'.
splitPathPieces :: Text -> [Text]
splitPathPieces = decodePathSegments . encodeUtf8

-- | Generate the URL for a template, given the parameters it captures.
--
-- Each piece is percent-encoded with 'encodePathPiece'. The result has both a
-- leading and a trailing slash, and 'splitPathPieces' turns it back into pieces
-- that match the template it came from.
usePathTemplateParams :: PathTemplate args -> RouteParams args -> Text
usePathTemplateParams pt' rp' = "/" <> go pt' rp'
  where
    go :: PathTemplate args' -> RouteParams args' -> Text
    go pt rp =
      case pt of
        PathEnd -> mempty
        (StaticPiece piece :/ rest) -> encodePathPiece piece <> "/" <> go rest rp
        (CapPiece :/ rest) ->
          case rp of
            (arg :-$ restParams) ->
              encodePathPiece (toUrlPiece arg) <> "/" <> go rest restParams

-- | Why matching a URL to a path template failed.
data TemplateMatchFailureMessage
  = CaptureFailed SomeTypeRep Text Text
  | StaticFailed Text Text
  | NotEnough
  deriving (Show, Eq, Ord, Generic)

-- | Why a template failed to match.
data TemplateFailure
  = ExtraPieces [Text]
  | MatchFailedAfter [Text] TemplateMatchFailureMessage
  deriving (Show, Eq, Ord, Generic)

splitFirstCapture :: PathTemplate (x ': xs) -> (PathTemplate '[x], PathTemplate xs)
splitFirstCapture = go id
  where
    go :: (PathTemplate '[x] -> PathTemplate '[x]) -> PathTemplate (x ': xs) -> (PathTemplate '[x], PathTemplate xs)
    go m = \case
      (c@CapPiece :/ r) -> (m (c :/ PathEnd), r)
      (s@(StaticPiece _) :/ r) ->
        go ((s :/) . m) r

removeFirstCapture :: PathTemplate (x ': xs) -> PathTemplate xs
removeFirstCapture = snd . splitFirstCapture

-- | Is this the end of a URL's pieces?
--
-- A single empty piece is the trailing slash of the URL, and ends the path just
-- like running out of pieces does. Note that this is only true in /final/
-- position: an empty piece with anything after it is a real, empty piece, and
-- matches nothing. 'Noided.Pathname.Internal.Router.foldrRouterOfMatches' makes
-- exactly the same distinction, and the two must agree.
endOfPathPieces :: [Text] -> Bool
endOfPathPieces [] = True
endOfPathPieces [x] = Text.null x
endOfPathPieces _ = False

-- | Try to match a given path template to a URL.
-- If the match is successful it will give you route params for that URL.
-- Otherwise, it will specify how and when matching failed.
--
-- This is the matcher behind 'Noided.Pathname.testUrlResult', so it has to
-- accept exactly the same URLs as the real router does: anything this reports
-- as a match must route, and anything it rejects must 404.
matchPathTemplate :: [Text] -> PathTemplate caps -> Either TemplateFailure (RouteParams caps)
matchPathTemplate = go []
  where
    go :: forall caps'. [Text] -> [Text] -> PathTemplate caps' -> Either TemplateFailure (RouteParams caps')
    go usedRev pieces template
      | endOfPathPieces pieces =
          case template of
            PathEnd -> pure RPNil
            (_ :/ _) -> Left $ MatchFailedAfter (reverse usedRev) NotEnough
    go _ r PathEnd = Left $ ExtraPieces r
    go usedRev (x : xs) (h :/ t) =
      case h of
        StaticPiece txt
          | txt == x -> go (x : usedRev) xs t
          | otherwise -> Left $ MatchFailedAfter (reverse usedRev) $ StaticFailed x txt
        CapPiece @r ->
          case parseUrlPiece x of
            Right p -> (p :-$) <$> go (x : usedRev) xs t
            Left err -> Left $ MatchFailedAfter (reverse usedRev) $ CaptureFailed (SomeTypeRep $ typeRep @r) x err
    -- Unreachable: 'endOfPathPieces' above covers an empty list, but GHC cannot
    -- see that through the guard.
    go usedRev [] (_ :/ _) = Left $ MatchFailedAfter (reverse usedRev) NotEnough

-- | GADT to match on if a template result existed or not.
data SomeTemplateResult where
  SomeTemplateResult ::
    -- | The tested template
    PathTemplate caps ->
    -- | Either a successful match or a failure
    Either TemplateFailure (RouteParams caps) ->
    SomeTemplateResult
