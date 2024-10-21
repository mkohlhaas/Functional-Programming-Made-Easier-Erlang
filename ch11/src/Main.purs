module Main where

import Data.Foldable (class Foldable, foldMap, foldl, foldlDefault, foldrDefault)
import Data.List (List(..), singleton, (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Data.Semigroup.Foldable (class Foldable1, foldl1)
import Data.Semiring (add, zero)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, class Semiring, Unit, discard, flip, negate, show, type (~>), ($), (<>), (>))

data Tree a = Leaf a | Node (Tree a) (Tree a)
newtype RFTree a = RFTree (Tree a) -- RightFirstTree; breadth-first search
newtype LFTree a = LFTree (Tree a) -- LeftFirstTree;    depth-first search

class ToList f where
  toList ∷ ∀ a. f a → List a

---------------
-- Functions --
---------------

reverse ∷ List ~> List
reverse = foldl (flip (:)) Nil

max ∷ ∀ a. Ord a ⇒ a → a → a
max x y = if x > y then x else y

findMax ∷ ∀ f a. Foldable f ⇒ Ord a ⇒ a → f a → a
findMax = foldl max

findMaxNE ∷ ∀ f a. Foldable1 f ⇒ Ord a ⇒ f a → a
findMaxNE = foldl1 max

-- Note: `Semiring` is a bit overkill as it also implies multiplication.
sum ∷ ∀ f a. Foldable f ⇒ Semiring a ⇒ f a → a
sum = foldl add zero

---------------
-- Instances --
---------------

-- Eta reducing `f` will result in a strange compiler error:
-- "The value of $foldableTree9 is undefined here, so this reference is not allowed."

instance Foldable Tree where
  foldl f = foldlDefault f
  foldr f = foldrDefault f
  foldMap f (Leaf a) = f a
  foldMap f (Node l r) = foldMap f l <> foldMap f r

instance Foldable LFTree where
  foldl f = foldlDefault f
  foldr f = foldrDefault f
  foldMap f (LFTree (Leaf a)) = f a
  foldMap f (LFTree (Node l r)) = foldMap f (LFTree l) <> foldMap f (LFTree r)

instance Foldable RFTree where
  foldl f = foldlDefault f
  foldr f = foldrDefault f
  foldMap f (RFTree (Leaf a)) = f a
  foldMap f (RFTree (Node l r)) = foldMap f (RFTree r) <> foldMap f (RFTree l)

instance ToList Tree where
  toList = foldMap singleton

instance ToList LFTree where
  toList = foldMap singleton

instance ToList RFTree where
  toList = foldMap singleton

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 11."
  log "Using folds!!!"
  log $ show $ reverse (10 : 20 : 30 : Nil) ------------------------------------------------------------ (30 : 20 : 10 : Nil)
  log $ show $ max (-1) 99 ----------------------------------------------------------------------------- 99
  log $ show $ max "aa" "z" ---------------------------------------------------------------------------- "z"
  log $ show $ findMax 0 (37 : 311 : -1 : 2 : 84 : Nil) ------------------------------------------------ 311 (0  is default value)
  log $ show $ findMax "" ("a" : "bbb" : "c" : Nil) ---------------------------------------------------- "c" ("" is default value)
  log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil)) ------------------------------ 311
  log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil)) ----------------------------------- "c"
  log $ show $ sum (1 : 2 : 3 : Nil) ------------------------------------------------------------------- 6
  log $ show $ sum (1.0 : 2.0 : 3.0 : Nil) ------------------------------------------------------------- 6.0
  log $ show $ sum [ 1, 2, 3 ] ------------------------------------------------------------------------- 6
  log $ show $ sum [ 1.0, 2.0, 3.0 ] ------------------------------------------------------------------- 6.0
  log $ show $ toList {-          -}  (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99)) ---- (5 : -1 : 14 : 99 : Nil)
  log $ show $ sum {-             -}  (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99)) ---- 117
  log $ show $ toList $ LFTree {- -}  (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99)) ---- (5 : -1 : 14 : 99 : Nil)
  log $ show $ sum $ LFTree {-    -}  (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99)) ---- 117
  log $ show $ toList $ RFTree {- -}  (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99)) ---- (99 : 14 : -1 : 5 : Nil)
  log $ show $ sum $ RFTree {-    -}  (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99)) ---- 117
