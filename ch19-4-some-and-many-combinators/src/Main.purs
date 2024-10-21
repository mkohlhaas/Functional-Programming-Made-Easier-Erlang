module Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none)
import Effect (Effect)
import Effect.Console (log)
import Parser (class ParserError, PError(..), Parser, constChar, constChar', digit, letter, parse', range')

---------------------------
-- Some and Many Parsers --
---------------------------

---------------------------------------------
-- Mutually recursive definition ------------
---------------------------------------------
-- "some" = "one" and "many" (+ in regexs) --
-- "many" = "some" or "none" (* in regexs) --
---------------------------------------------

-- PureScript is a strict language. As both parsers are defined in terms of the other we have to
-- write a Lazy instance for Parser. At least one of "some" or "many" has to be lazy.
-- We'll choose "some" to be lazy.

-- First we write specialized parsers that will be later expressed from generic ones.
-- E.g specialized `some'` will be expressed from generic `some`.

--------------------------------------------------------------------------------------------
-- 1. Write "some'" and "many'" in a general form using Arrays from its definitions above --
--------------------------------------------------------------------------------------------
-- Do not worry about mutual recursion at this point. It will blow the stack but we'll fix it shortly.

-- some' ∷ ∀ e a. ParserError e ⇒ Parser e a → Parser e (Array a)
-- some' p = do
--   a ← p
--   as ← many' p
--   pure $ a : as

-- many' ∷ ∀ e a. ParserError e ⇒ Parser e a → Parser e (Array a)
-- many' p = some' p <|> pure []

------------------------------------------
-- 2. Create a Lazy instance for Parser --
------------------------------------------

-- see src/Parser.purs

----------------------------------------------------------
-- 3. Make "some'" lazy by deferring the "many'" parser --
----------------------------------------------------------

-- some' ∷ ∀ e a. ParserError e ⇒ Parser e a → Parser e (Array a)
-- some' p = do
--   a ← p
--   as ← defer (const $ many' p)
--   pure $ a : as

-- many' ∷ ∀ e a. ParserError e ⇒ Parser e a → Parser e (Array a)
-- many' p = some' p <|> pure []

----------------------------------------------------
-- 4. Write specific versions for Char and String --
----------------------------------------------------

-- some'' ∷ ∀ e. ParserError e ⇒ Parser e Char → Parser e String
-- some'' p = fromCharArray <$> some' p

-- many'' ∷ ∀ e. ParserError e ⇒ Parser e Char → Parser e String
-- many'' p = fromCharArray <$> many' p

----------------------------------------------------------------------
-- 5. Write a parser called "digits" that parses at least one digit --
----------------------------------------------------------------------

digits ∷ ∀ e. ParserError e ⇒ Parser e String
digits = some'' digit

-----------------------------------------------------------------------------------------------------------------
-- 6. Write a parser for the following regular expression (including capturing) in do notation (monadic style) --
-----------------------------------------------------------------------------------------------------------------
-- (\d{1,4}), ([a-zA-Z ]+)([0-9]*)

-- We store the capture groups in an array.
regexM ∷ ∀ e. ParserError e ⇒ Parser e (Array String)
regexM = do
  cg1 ← range' 1 4 digit
  constChar ','
  constChar ' '
  cg2 ← some'' (letter <|> constChar' ' ')
  cg3 ← many'' digit
  pure $ [cg1, cg2, cg3]

---------------------------------------------------
-- 7. Write the same parser in applicative style --
---------------------------------------------------

regexA ∷ ∀ e. ParserError e ⇒ Parser e (Array String)
regexA = (\cg1 cg2 cg3 → [cg1, cg2, cg3]) <$> range' 1 4 digit <* constChar ',' <* constChar ' ' <*> some'' (letter <|> constChar' ' ') <*> many'' digit

-----------------------------------------------------
-- 8. All tests should work now. Let's generalize! --
-----------------------------------------------------

-- Generalize some and many and express the primed versions with these generic parsers.
-- Comment out all the previous primed versions.

-- Write generic some and many!
-- Pass a cons function parameter because Unfoldable does not have one.

-- some ∷ ∀ e a f. ParserError e ⇒ Unfoldable f ⇒ (a → f a → f a) → Parser e a → Parser e (f a)
-- some cons p = do
--   a ← p
--   as ← defer (const $ many cons p)
--   pure $ cons a as

-- many ∷ ∀ e a f. ParserError e ⇒ Unfoldable f ⇒ (a → f a → f a) → Parser e a → Parser e (f a)
-- many cons p = some cons p <|> pure none

-- Write primed versions from the generic versions!

-- some' ∷ ∀ e a. ParserError e ⇒ Parser e a → Parser e (Array a)
-- some' p = some (:) p

-- many' ∷ ∀ e a. ParserError e ⇒ Parser e a → Parser e (Array a)
-- many' p = many (:) p

-- some'' ∷ ∀ e. ParserError e ⇒ Parser e Char → Parser e String
-- some'' p = fromCharArray <$> some' p

-- many'' ∷ ∀ e. ParserError e ⇒ Parser e Char → Parser e String
-- many'' p = fromCharArray <$> many' p

-----------------------------------------------------
-- 9. `some` will never return an empty Unfoldable --
-----------------------------------------------------

-- some ∷ ∀ e a f. ParserError e ⇒ Unfoldable f ⇒ (a → f a → f a) → Parser e a → Parser e (NonEmpty f a)
-- some cons p = do
--   a ← p
--   as ← defer (const $ many cons p)
--   pure $ (:|) a as

-- The compiler will complain about "many".
-- We have to transform a `NonEmpty` to something that is not `NonEmpty`. ;-)
-- Use `fromNonEmpty` to fix the issue. Look it up on Pursuit.

-- many ∷ ∀ e a f. ParserError e ⇒ Unfoldable f ⇒ (a → f a → f a) → Parser e a → Parser e (f a)
-- many cons p = fromNonEmpty cons <$> some cons p <|> pure none

-- Write new primed versions. Comment out their previous versions.

some' ∷ ∀ e a. ParserError e ⇒ Parser e a → Parser e (Array a)
some' p = fromNonEmpty (:) <$> some (:) p

many' ∷ ∀ e a. ParserError e ⇒ Parser e a → Parser e (Array a)
many' p = many (:) p

some'' ∷ ∀ e. ParserError e ⇒ Parser e Char → Parser e String
some'' p = fromCharArray <$> some' p

many'' ∷ ∀ e. ParserError e ⇒ Parser e Char → Parser e String
many'' p = fromCharArray <$> many' p

-----------------------------------------------------------------------------
-- 10. Make the resulting combinators "some" and "many" even more general! --
-----------------------------------------------------------------------------

-- Let's use any Monad instead of just our Parser and adapt the constraints.
-- Comment out their previous versions. Leave the primed versions alone.

some ∷ ∀ m f a. Alt m ⇒ Lazy (m (f a)) ⇒ Monad m ⇒ Unfoldable f ⇒ (a → f a → f a) → m a → m (NonEmpty f a)
some cons p = do
  a ← p
  as ← defer (const $ many cons p)
  pure $ (:|) a as

many ∷ ∀ a f m. Alt m ⇒ Lazy (m (f a)) ⇒ Monad m ⇒ Unfoldable f ⇒ (a → f a → f a) → m a → m (f a)
many cons p = fromNonEmpty cons <$> some cons p <|> pure none

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 19 - Some and Many Parsers."
  log $ show $ parse' (some' digit) "2343423423abc" == (Right (Tuple "abc" ['2','3','4','3','4','2','3','4','2','3']))
  log $ show $ parse' (some' digit) "_2343423423abc" == (Left (InvalidChar "digit"))
  log $ show $ parse' (many' digit) "_2343423423abc" == (Right (Tuple "_2343423423abc" []))
  log $ show $ parse' (some'' digit) "2343423423abc" == (Right (Tuple "abc" "2343423423"))
  log $ show $ parse' (some'' digit) "_2343423423abc" == (Left (InvalidChar "digit"))
  log $ show $ parse' (many'' digit) "_2343423423abc" == (Right (Tuple "_2343423423abc" ""))
  log $ show $ parse' digits "2343423423abc" == (Right (Tuple "abc" "2343423423"))
  log $ show $ parse' digits "_2343423423abc" == (Left (InvalidChar "digit"))
  log $ show $ parse' regexM "17, some words" == (Right (Tuple "" ["17","some words",""]))
  log $ show $ parse' regexM "5432, some more words1234567890" == (Right (Tuple "" ["5432","some more words","1234567890"]))
  log $ show $ parse' regexA "17, some words" == (Right (Tuple "" ["17","some words",""]))
  log $ show $ parse' regexA "5432, some more words1234567890" == (Right (Tuple "" ["5432","some more words","1234567890"]))
