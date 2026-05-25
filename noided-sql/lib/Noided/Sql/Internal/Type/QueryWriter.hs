{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.QueryWriter
  ( QueryWriter,
    runQueryWriter,
    renderQueryWriter,
    writeText,
    writeSyntax,
    toUniqueAlias,
    toQuotedUniqueAlias,
    delayWriting,
    syntaxSubquery,
  )
where

import Control.Monad.State.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict (StateT (StateT), evalStateT)
import Control.Monad.Trans.Writer.CPS (Writer, runWriter)
import Control.Monad.Writer.Class
import Data.Map.Strict qualified as Map
import Data.String
import Data.Text (Text, pack)
import Noided.Sql.Internal.Type.Syntax
import Optics.Core

type QueryWriterState = Map.Map Text Int

newtype QueryWriter a
  = QueryWrite {getQueryWrite :: ReaderT Word (StateT QueryWriterState (Writer Syntax)) a}
  deriving (Functor, Applicative, Monad) via (ReaderT Word (StateT QueryWriterState (Writer Syntax)))

syntaxSubquery :: QueryWriter a -> Syntax
syntaxSubquery qr = Syn $ \depth ->
  let newDepth = depth + 1
      (_, Syn newSyn) = runQueryWriterAtDepth newDepth qr
   in newSyn newDepth

-- | Delay writing until later.
delayWriting :: QueryWriter a -> QueryWriter (a, Syntax)
delayWriting qs =
  toQueryWrite $
    censor mempty (listen (getQueryWrite qs))

runQueryWriterAtDepth :: Word -> QueryWriter a -> (a, Syntax)
runQueryWriterAtDepth depth a =
  runWriter
    ( evalStateT
        (runReaderT (getQueryWrite a) depth)
        mempty
    )

runQueryWriter :: QueryWriter a -> (a, Syntax)
runQueryWriter = runQueryWriterAtDepth 0

renderQueryWriter :: QueryWriter a -> Syntax
renderQueryWriter = snd . runQueryWriter

-- | Provided for nicer syntax when writing stuff.
instance (a ~ ()) => IsString (QueryWriter a) where
  fromString = writeSyntax . fromString

writeText :: Text -> QueryWriter ()
writeText = writeSyntax . syntaxFromText

-- | Write some syntax to the output buffer.
writeSyntax :: Syntax -> QueryWriter ()
writeSyntax syn = toQueryWrite $ tell syn

toQueryWrite :: ReaderT Word (StateT QueryWriterState (Writer Syntax)) a -> QueryWriter a
toQueryWrite = QueryWrite

-- | Qualify a name to be unique within a given scope.
-- Note that if the syntax is nested, an appropriate nesting suffix will also be added.
-- Does not write anything to the query.
toUniqueAlias :: Text -> QueryWriter Syntax
toUniqueAlias alias = toQueryWrite $ do
  r <- get
  let nestedAlias =
        case Map.lookup alias r of
          Nothing -> alias
          Just i -> alias <> pack ("_" <> show i)
  modify (at alias % non 0 %~ (+ 1))
  nesting <- ask
  return $ Syn $ \_ ->
    let suffix = if nesting > 0 then pack ("_nested_" <> show nesting <> "_deep") else mempty
     in pure (RawSyntax $ nestedAlias <> suffix)

-- | Like 'toUniqueAlias', but also adds quotes around the value.
-- Useful when qualifying table names and such.
toQuotedUniqueAlias :: Text -> QueryWriter Syntax
toQuotedUniqueAlias alias = do
  un <- toUniqueAlias alias
  return $ "\"" <> un <> "\""
