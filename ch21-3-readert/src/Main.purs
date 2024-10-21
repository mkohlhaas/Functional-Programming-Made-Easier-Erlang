module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

-- Define data type for ReaderT.

newtype ReaderT ∷ ∀ k. Type → (k → Type) → k → Type
newtype ReaderT r m a = ReaderT (r → m a)

-- Write runReaderT function.

runReaderT ∷ ∀ r m a. ReaderT r m a → r → m a
runReaderT (ReaderT f) = f

-- Implement Functor instance for ReaderT.

instance Functor m ⇒ Functor (ReaderT r m) where
  map f (ReaderT x) = ReaderT \r → f <$> x r

-- Implement Apply instance for ReaderT.

instance Apply m ⇒ Apply (ReaderT r m) where
  apply (ReaderT f) (ReaderT x) = ReaderT \r → f r <*> x r

-- Implement Applicative instance for ReaderT.

instance Applicative m ⇒ Applicative (ReaderT r m) where
  pure = ReaderT <<< const <<< pure

-- Implement Bind instance for ReaderT.

-- bind ∷ ∀ a b. m a → (a → m b) → m b
-- instance Bind m ⇒ Bind (ReaderT r m) where
--   bind (ReaderT x) f = ReaderT \r → do
--     x' ← x r
--     runReaderT (f x') r

instance Bind m ⇒ Bind (ReaderT r m) where
  bind (ReaderT x) f = ReaderT \r → x r >>= \x' → runReaderT (f x') r

-- Implement Monad instance for ReaderT.

instance Monad m ⇒ Monad (ReaderT r m)

main ∷ Effect Unit
main = do
  log "Coding ReaderT"
