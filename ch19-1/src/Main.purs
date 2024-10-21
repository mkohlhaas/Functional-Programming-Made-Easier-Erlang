module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

-----------
-- Maybe --
-----------
data Maybe a = Nothing | Just a

derive instance Generic (Maybe a) _

instance Show a ⇒ Show (Maybe a) where
  show = genericShow

instance Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance Apply Maybe where
  apply Nothing _ = Nothing
  apply (Just f) x = f <$> x

-- instance Apply Maybe where
--   apply (Just f) (Just x) = Just $ f x
--   apply _ _ = Nothing

instance Applicative Maybe where
  pure = Just

instance Bind Maybe where
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance Monad Maybe

------------
-- Either --
------------
data Either a b = Left a | Right b

derive instance Generic (Either a b) _

instance (Show a, Show b) ⇒ Show (Either a b) where
  show = genericShow

instance Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right x) = Right $ f x

instance Apply (Either a) where
  apply (Left x) _ = Left x
  apply (Right f) x = f <$> x

instance Applicative (Either a) where
  pure = Right

instance Bind (Either a) where
  bind (Left x) _ = Left x
  bind (Right x) f = f x

instance Monad (Either a)

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 19."
  log "-----------------"
  log "-- Maybe Monad --"
  log "-----------------"
  log $ show $ Just (_ * 10) <*> Just 20 ------------------------------------- (Just 200)
  log $ show $ Just (_ * 10) <*> pure 20 ------------------------------------- (Just 200)
  log $ show $ Just 20 >>= pure <<< (_ * 10) --------------------------------- (Just 200)
  log $ show do -------------------------------------------------------------- (Just 200)
    x ← Just 20
    let y = x * 10
    pure y
  log $ show $ Just 20 >>= const Nothing >>= \y → Just $ y + 42 -------------- Nothing
  log $ show do -------------------------------------------------------------- Nothing
    _ ← Just 20
    y ← Nothing
    pure $ y + 42
  log "------------------"
  log "-- Either Monad --"
  log "------------------"
  log $ show $ Right (_ * 10) <*> (Right 20 ∷ Either Unit _) ----------------- (Right 200)
  log $ show $ Right (_ * 10) <*> (pure 20 ∷ Either Unit _) ------------------ (Right 200)
  log $ show $ (Right 20 ∷ Either Unit _) >>= pure <<< (_ * 10) -------------- (Right 200)
  log $ show do -------------------------------------------------------------- (Right 200)
    x ← Right 20 ∷ Either Unit _
    let y = x * 10
    pure y
  log $ show $ Right 20 >>= const (Left "error") >>= \y → Right $ y + 42 ----- (Left "error")
  log $ show do -------------------------------------------------------------- (Left "error")
    _ ← Right 20
    y ← Left "error"
    pure $ y + 42
