module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- Define data type for WriterT.
newtype WriterT w m a = WriterT (m (Tuple a w))

-- Write runWriterT function.
runWriterT ∷ ∀ w m a. WriterT w m a → m (Tuple a w)
runWriterT (WriterT m) = m

-- Implement Apply instance for WriterT.

instance (Monad m, Semigroup w) ⇒ Apply (WriterT w m) where
  apply (WriterT f) (WriterT x) = WriterT do
    Tuple f' w ← f
    Tuple x' w' ← x
    pure $ Tuple (f' x') (w <> w')

-- Implement Applicative instance for WriterT.

instance (Monad m, Monoid w) ⇒ Applicative (WriterT w m) where
  pure x = WriterT $ pure $ Tuple x mempty

-- Implement Functor instance for WriterT.

instance Functor m ⇒ Functor (WriterT w m) where
  map f (WriterT mx) = WriterT $ mx <#> \(Tuple x w) → Tuple (f x) w

-- Implement Bind instance for WriterT.

instance (Monad m, Monoid w) ⇒ Bind (WriterT w m) where
  bind (WriterT x) f = WriterT do
    Tuple x' w ← x
    Tuple x'' w' ← runWriterT $ f x'
    pure $ Tuple x'' (w <> w')

-- Implement Monad instance for WriterT.

instance (Monoid w, Monad m) ⇒ Monad (WriterT w m)

main ∷ Effect Unit
main = do
  log "Coding WriterT"
