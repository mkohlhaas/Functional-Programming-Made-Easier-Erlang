module Main where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Generic.Rep (class Generic)
import Data.Int (even, odd)
import Data.List (List(..), (:))
import Data.Profunctor (class Profunctor, dimap)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Effect (Effect)
import Effect.Console (log)

-------------------------------------------------------
---------------------- Predicate ----------------------
-------------------------------------------------------

-- What is the polarity of the polymorphic parameter a ?
-- What kind of functor do we need ?
data Predicate a = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) = f

instance Contravariant Predicate where
  cmap f (Predicate g) = Predicate (g <<< f)

--------------------------------------------------------
---------------------- Moore Machine -------------------
--------------------------------------------------------

-- s = state type, a = input type, b = output type

-- s = initial state
-- s -> b = output function
-- s -> a -> s = transition function

--             output               transition function
--             |                    |
--           input         output function
--           | |           |        |
--         state         initial state
--         | | |         | |        |
data Moore s a b = Moore s (s -> b) (s -> a -> s)

-- What are the polarities of the polymorphic parameters a and b ?
-- What kind of functor do we need ?
-- What is the functor?

data OvenState = Off | Bake | Idling                             -- state type
data InputSignal = BakePressed | OffPressed | TooHot | TooCold   -- input type
data Heat = HeatOn | HeatOff                                     -- output type (each state has an associated output value)

-- output function
output :: OvenState -> Heat
output Off = HeatOff
output Bake = HeatOn
output Idling = HeatOff

-- transition function
transition :: OvenState -> InputSignal -> OvenState
transition Off BakePressed = Bake
transition Bake OffPressed = Off
transition Bake TooHot = Idling
transition Idling TooCold = Bake
transition Idling OffPressed = Off
transition s _ = s

oven :: Moore OvenState InputSignal Heat
oven = Moore Off output transition

derive instance Generic Heat _

instance Show Heat where
  show = genericShow

runMooreMachine :: ∀ b s a f. Foldable f => Moore s a b -> f a -> b
runMooreMachine (Moore initialState outputFn transitionFn) states = outputFn $ foldl transitionFn initialState states

adder :: ∀ a. Semiring a => Moore a a a
adder = Moore zero identity (+)

instance Profunctor (Moore s) where
  dimap f g (Moore initState outputFn transitionFn) = Moore initState (outputFn >>> g) (\state -> transitionFn state <<< f)

sizer :: Moore Int String String
sizer = dimap length (\n -> "Size is " <> show n) adder

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 15."
  log "------------ Predicates ------------"
  log $ show $ runPredicate (Predicate even) 10                                                             -- true
  log $ show $ runPredicate (Predicate odd) 11                                                              -- true
  log $ show $ runPredicate (Predicate even) 11                                                             -- false
  log $ show $ runPredicate (Predicate odd) 10                                                              -- false
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10                                               -- true
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10                                                -- true
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10                                               -- false
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10                                                -- false
  log "--------- Moore Machines -----------"
  log $ show $ runMooreMachine oven [ BakePressed, OffPressed, BakePressed, TooHot, TooCold, OffPressed ]   -- HeatOff
  log $ show $ runMooreMachine adder [ 1, 2, 3 ]                                                            -- 6
  log $ show $ runMooreMachine adder (1.0 : 2.0 : 3.0 : Nil)                                                -- 6.0
  log $ show $ runMooreMachine sizer [ "This", "is", "the", "test" ]                                        -- "Size is 13"
