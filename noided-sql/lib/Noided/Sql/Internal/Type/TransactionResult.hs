{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

module Noided.Sql.Internal.Type.TransactionResult where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Classes (Eq1 (..), Eq2 (..), Show1 (..), Show2 (..))
import GHC.Generics (Generic)
import Hasql.Errors (SessionError)

-- | An error type, representing either a session error, a transaction error, or a good result.
data TransactionResult e a = SessionErr SessionError | TransactErr e | TransactOK a
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance Applicative (TransactionResult e) where
  pure = TransactOK
  (TransactOK f) <*> (TransactOK a) = TransactOK (f a)
  (TransactErr e) <*> _ = TransactErr e
  _ <*> (TransactErr e) = TransactErr e
  (SessionErr e) <*> _ = SessionErr e
  _ <*> (SessionErr e) = SessionErr e

instance Monad (TransactionResult e) where
  (TransactOK a) >>= f = f a
  (TransactErr e) >>= _ = TransactErr e
  (SessionErr e) >>= _ = SessionErr e

instance Bifunctor TransactionResult where
  bimap _ _ (SessionErr e) = SessionErr e
  bimap f _ (TransactErr e) = TransactErr (f e)
  bimap _ g (TransactOK a) = TransactOK (g a)

instance Bifoldable TransactionResult where
  bifoldMap _ _ (SessionErr _) = mempty
  bifoldMap f _ (TransactErr e) = f e
  bifoldMap _ g (TransactOK a) = g a

instance Bitraversable TransactionResult where
  bitraverse _ _ (SessionErr e) = pure (SessionErr e)
  bitraverse f _ (TransactErr e) = TransactErr <$> f e
  bitraverse _ g (TransactOK a) = TransactOK <$> g a

instance Eq2 TransactionResult where
  liftEq2 _ _ (SessionErr e1) (SessionErr e2) = e1 == e2 -- SessionError has Eq instance?
  liftEq2 f _ (TransactErr e1) (TransactErr e2) = f e1 e2
  liftEq2 _ g (TransactOK a1) (TransactOK a2) = g a1 a2
  liftEq2 _ _ _ _ = False

instance (Eq e) => Eq1 (TransactionResult e) where
  liftEq = liftEq2 (==)

instance Show2 TransactionResult where
  liftShowsPrec2 _ _ _ _ _ (SessionErr e) = showString "SessionErr " . shows e
  liftShowsPrec2 sp1 _ _ _ d (TransactErr e) = showParen (d > 10) $ showString "TransactErr " . sp1 11 e
  liftShowsPrec2 _ _ sp2 _ d (TransactOK a) = showParen (d > 10) $ showString "TransactOK " . sp2 11 a

instance (Show e) => Show1 (TransactionResult e) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
