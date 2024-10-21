module Main where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Functor, class Show, Unit, discard, flip, identity, map, show, ($), (*), (/), (<$>), (<<<), (<>), (==))

-------------------------------------
-- Define Type Class for Bifunctor --
-------------------------------------

class Bifunctor f where
  bimap ∷ ∀ a b c d. (a → c) → (b → d) → f a b → f c d

rmap ∷ ∀ a b d f. Bifunctor f ⇒ (b → d) → f a b → f a d
rmap = bimap identity

lmap ∷ ∀ a b c f. Bifunctor f ⇒ (a → c) → f a b → f c b
lmap = flip bimap identity

------------------------------------------------------------
-- Define Types for Maybe, Either and Tuple - No imports! --
------------------------------------------------------------

data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
data Tuple a b = Tuple a b
data Threeple a b c = Threeple a b c

---------------
-- Instances --
---------------

--------
-- Eq --
--------

derive instance Eq a ⇒ Eq (Maybe a)
derive instance (Eq a, Eq b) ⇒ Eq (Tuple a b)

-------------
-- Generic --
-------------

derive instance Generic (Maybe a) _
derive instance Generic (Either a b) _
derive instance Generic (Tuple a b) _
derive instance Generic (Threeple a b c) _

----------
-- Show --
----------

instance Show a ⇒ Show (Maybe a) where
  show = genericShow

instance (Show a, Show b) ⇒ Show (Either a b) where
  show = genericShow

instance (Show a, Show b) ⇒ Show (Tuple a b) where
  show = genericShow

instance (Show a, Show b, Show c) ⇒ Show (Threeple a b c) where
  show = genericShow

-------------
-- Functor --
-------------

instance Functor Maybe where
  map _ Nothing = Nothing
  map f (Just a) = Just $ f a

instance Functor (Either a) where
  map _ (Left a) = Left a
  map f (Right b) = Right $ f b

instance Functor (Tuple a) where
  map f (Tuple a b) = Tuple a (f b)

instance Functor (Threeple a b) where
  map f (Threeple a b c) = Threeple a b (f c)

---------------
-- Bifunctor --
---------------

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)

instance Bifunctor Tuple where
  bimap f g (Tuple a b) = Tuple (f a) (g b)

instance Bifunctor (Threeple a) where
  bimap f g (Threeple a b c) = Threeple a (f b) (g c)

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 13."
  log $ show $ (_ / 2) <$> Just 10 --------------------------------------------------------------------------------- (Just 5)
  log $ show $ (_ / 2) <$> Nothing --------------------------------------------------------------------------------- Nothing
  log $ show $ (_ / 2) <$> (Right 10 ∷ Either Unit _) -------------------------------------------------------------- (Right 5)
  log $ show $ (_ / 2) <$> Left "error reason" --------------------------------------------------------------------- (Left "error reason")
  log $ show $ (_ / 2) <$> Tuple 10 20 ----------------------------------------------------------------------------- (Tuple 10 10)
  log $ show $ (_ / 2) <$> Threeple 10 20 40 ----------------------------------------------------------------------- (Threeple 10 20 20)
  log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing ∷ Maybe Unit)) ----------- Maybe Identity for Nothing: true
  log $ show $ "Maybe Identity for Just: " <> show ((identity <$> Just [ 1, 2 ]) == Just [ 1, 2 ]) ----------------- Maybe Identity for Just: true
  let
    g x = x * 2
    f x = x * 3
  log $ show $ "Maybe Composition for Nothing: " <> show ((map (f <<< g) Nothing) == (map f <<< map g) Nothing) ---- "Maybe Composition for Nothing: true"
  log $ show $ "Maybe Composition for Just: " <> show ((map (f <<< g) (Just 60)) == (map f <<< map g) (Just 60)) --- "Maybe Composition for Just: true"
  log $ show $ "Tuple Identity: " <> show ((identity <$> Tuple 10 20) == Tuple 10 20) ------------------------------ "Tuple Identity: true"
  log $ show $ "Tuple Composition : " <> show ((map (f <<< g) (Tuple 10 20)) == (map f <<< map g) (Tuple 10 20)) --- "Tuple Composition : true"
  log $ show $ rmap (_ * 2) $ Left "error reason" ------------------------------------------------------------------ (Left "error reason")
  log $ show $ lmap toUpper $ (Left "error reason" ∷ Either String Unit) ------------------------------------------- (Left "ERROR REASON")
  log $ show $ rmap (_ * 2) $ (Right 10 ∷ Either Unit Int) --------------------------------------------------------- (Right 20)
  log $ show $ lmap toUpper $ Right 10 ----------------------------------------------------------------------------- (Right 10)
  log $ show $ rmap (_ * 2) $ Tuple 80 40 -------------------------------------------------------------------------- (Tuple 80 80)
  log $ show $ lmap (_ / 2) $ Tuple 80 40 -------------------------------------------------------------------------- (Tuple 40 40)
  log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40 ----------------------------------------------------------------- (Tuple 40 80)
  log $ show $ rmap (_ * 2) $ Threeple 99 80 40 -------------------------------------------------------------------- (Threeple 99 80 80)
  log $ show $ lmap (_ / 2) $ Threeple 99 80 40 -------------------------------------------------------------------- (Threeple 99 40 40)
  log $ show $ bimap (_ / 2) (_ * 2) $ Threeple 99 80 40 ----------------------------------------------------------- (Threeple 99 40 80)
