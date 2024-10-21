module Main where

import Prelude

import Control.Monad.Except.Trans (class MonadError, class MonadThrow, ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Control.Monad.State.Trans (class MonadState, get, put)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Trans (class MonadTell, WriterT, runWriterT, tell)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- 1. Write StateT type definition.

newtype StateT s m a = StateT (s → m (Tuple a s))

-- 2. Write runStateT.

runStateT ∷ ∀ s m a. StateT s m a → s → m (Tuple a s)
runStateT (StateT f) = f

-- 3. Write Functor instance.

instance Monad m ⇒ Functor (StateT s m) where
  map f (StateT g) = StateT $ \s → do
    Tuple a s' ← g s
    pure $ Tuple (f a) s'

-- 4. Write a Functor instance with only a Functor constraint.

-- instance Functor m ⇒ Functor (StateT s m) where
--   map f (StateT g) = StateT $ \s → g s <#> \(Tuple a b) → Tuple (f a) b

-- 5. Write Apply instance.

-- monadic style
instance Monad m ⇒ Apply (StateT s m) where
  apply (StateT f) (StateT g) = StateT $ \s → do
    Tuple f' s' ← f s
    Tuple a s'' ← g s'
    pure $ Tuple (f' a) s''

-- cheating style :-)
-- instance Monad m ⇒ Apply (StateT s m) where
--   apply = ap

-- 6. Write Applicative instance.

instance Monad m ⇒ Applicative (StateT s m) where
  pure a = StateT \s → pure $ Tuple a s

-- 7. Write Bind instance.

instance Monad m ⇒ Bind (StateT s m) where
  bind (StateT f) g = StateT \s → f s >>= \(Tuple a s') → g a # \(StateT h) → h s'

-- alternative
-- instance Monad m ⇒ Bind (StateT s m) where
--   bind (StateT x) f = StateT \s → x s >>= \(Tuple x' s') → runStateT (f x') s'

-- with do notation:
-- bind (StateT f) g = StateT $ \s → do
--    Tuple a s' ← f s
--    let StateT h = g a
--    h s'

-- 8. Write Monad instance.

instance Monad m ⇒ Monad (StateT s m)

-- 9. Write MonadState instance.

instance Monad m ⇒ MonadState s (StateT s m) where
  state f = StateT $ pure <<< f

-- 10. Write MonadTrans instance.

-- instance Monad m ⇒ MonadTrans (StateT s) where
--   lift m = StateT $ \s → m >>= \a → pure $ Tuple a s

instance MonadTrans (StateT s) where
  lift m = StateT $ \s → do
    a ← m
    pure $ Tuple a s

-- alternative
-- instance MonadTrans (StateT s) where
--   lift x = StateT \s → x <#> \x' → Tuple x' s

-- 11. Write MonadAsk instance.

-- instance MonadAsk r m ⇒ MonadAsk r (StateT s m) where
--   ask = StateT $ \s → do
--     r ← ask
--     pure $ Tuple r s

-- alternative
-- instance MonadAsk r m ⇒ MonadAsk r (StateT s m) where
--   ask = StateT \s → ask <#> \r → Tuple r s

-- 12. Write MonadTell instance.

-- instance MonadTell w m ⇒ MonadTell w (StateT s m) where
--   tell w = StateT $ \s → do
--     tell w
--     pure $ Tuple unit s

-- alternative
-- instance MonadTell w m ⇒ MonadTell w (StateT s m) where
--   tell w = StateT \s → tell w <#> (const $ Tuple unit s)

-- 13. Write monadAsk and monadTell instances in terms of MonadTrans, i.e. with lift.

instance MonadAsk r m ⇒ MonadAsk r (StateT s m) where
  ask = lift ask

instance MonadTell w m ⇒ MonadTell w (StateT s m) where
  tell = lift <<< tell

-- 14. Write MonadThrow instance.

-- instance MonadThrow e m ⇒ MonadThrow e (StateT s m) where
--   throwError e = StateT $ \s → do
--      a ← throwError e
--      pure $ Tuple a s

instance MonadThrow e m ⇒ MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

-- 15. Write MonadError instance.

-- Notice that we have to pass the same state twice!

instance MonadError e m ⇒ MonadError e (StateT s m) where
  catchError (StateT f) g = StateT \s → catchError (f s) \e → runStateT (g e) s

-------------
-- Testing --
-------------

-- This is our Monad stack:

-- Note: ExceptT is at the top of the stack to make sure the log and state are not lost in case of an error.
--       And catchError for other Monad transformers are not called which are faulty by design.

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a

-- 16. Write runApp to run AppStack.

runApp ∷ ∀ e a w s m. s → ExceptT e (WriterT w (StateT s m)) a → m (StackResult e w s a)
runApp s = runExceptT >>> runWriterT >>> flip runStateT s

-- 17. Factor out the return type, call it StackResult and update runApp.

type StackResult e w s a = (Tuple (Tuple (Either e a) w) s)

---------------------
-- Helper function --
---------------------

logM ∷ ∀ m. MonadTell (Array String) m ⇒ String → m Unit
logM s = tell [ s <> "\n" ]

validate ∷ Int → AppM
validate n = when (n == 0) $ void $ throwError "WE CANNOT HAVE A 0 STATE!"

-- our specific monad stack
type AppM = AppStack String (Array String) Int Unit

app1 ∷ AppM
app1 = do
  logM "Starting App..."
  n ← get
  when (n == 0) $ void $ throwError "WE CANNOT HAVE A ZERO STATE!"
  put $ n + 1
  logM "Incremented State"
  pure unit

app2 ∷ AppM
app2 = do
  logM "Starting App..."
  n ← get
  catchError (validate n)
    ( \err → do
        tell [ "We encountered an error: " <> err ]
        put 1
    )
  put $ n + 1
  logM "Incremented State"
  pure unit

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 21."
  result1 ← runApp 0 app1
  log $ show $ result1 == (Tuple (Tuple (Left "WE CANNOT HAVE A ZERO STATE!") [ "Starting App...\n" ]) 0)
  result2 ← runApp 99 app1
  log $ show $ result2 == (Tuple (Tuple (Right unit) [ "Starting App...\n", "Incremented State\n" ]) 100)
  result3 ← runApp 0 app2
  log $ show $ result3 == (Tuple (Tuple (Right unit) [ "Starting App...\n", "We encountered an error: WE CANNOT HAVE A 0 STATE!", "Incremented State\n" ]) 1)
  result4 ← runApp 99 app2
  log $ show $ result4 == (Tuple (Tuple (Right unit) [ "Starting App...\n", "Incremented State\n" ]) 100)
