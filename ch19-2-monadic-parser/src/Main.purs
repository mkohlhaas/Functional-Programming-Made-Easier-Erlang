module Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicateA)
import Effect (Effect)
import Effect.Console (log)

-- The Parsing State is going to need to be passed from Parser to Parser, i.e. when the current Parser is done,
-- it passes what’s left of the String to the next Parser who takes a stab at parsing what's left. Also, if a
-- single Parser in the chain were to fail, we want to short-circuit the parsing and return the error with
-- some useful information as to what went wrong.

---------------------------------
-- Data Types and Type Classes --
---------------------------------

-- e = error type
-- a = return type

class ParserError e where
  eof ∷ e
  invalidChar ∷ String → e

data PError = EOF | InvalidChar String -- application specific parse error type
type ParserState a = Tuple String a -- left-over string and parsed value
type ParseFunction e a = ParserError e ⇒ String → Either e (ParserState a)
newtype Parser e a = Parser (ParseFunction e a)
data Threeple a b c = Threeple a b c

instance ParserError PError where
  eof = EOF
  invalidChar = InvalidChar

-- The reason for double mapping is because we have a Tuple inside of an Either - we have to make two hops.
instance Functor (Parser e) where
  map f a = Parser \str → map f <$> parse a str

-- instance Apply (Parser e) where
--   apply p1 p2 =
--     Parser $ \str → case parse p1 str of
--       Left err → Left err
--       Right (Tuple str' f) → case parse p2 str' of
--         Left err' → Left err'
--         Right (Tuple str'' a) → Right (Tuple str'' (f a))

-- exactly the same as: apply = ap
instance Apply (Parser e) where
  apply p1 p2 = do
    f ← p1
    a ← p2
    pure $ f a

instance Applicative (Parser e) where
  pure a = Parser \str → Right $ Tuple str a

-- 1. Create a Bind instance for Parser.
instance Bind (Parser e) where
  bind m f = Parser \str → case parse m str of
    Left err → Left err
    Right (Tuple str' a) → parse (f a) str'

-- Using Either's bind:
-- instance Bind (Parser e) where
--   bind m f = Parser \str -> do
--     Tuple str' a ← parse m str
--     parse (f a) str'

-- 2. Create a Monad instance for Parser and rewrite Apply (Parser e) in do notation.
instance Monad (Parser e)

-- 3. Create Alt instance for Parser.
instance Alt (Parser e) where
  alt p1 p2 = Parser \str -> parse p1 str <|> parse p2 str  -- leverages Either's Alt instance

------------
-- Parser --
------------

parse ∷ ∀ e a. Parser e a → ParseFunction e a
parse (Parser f) = f

parse' ∷ ∀ a. Parser PError a → ParseFunction PError a
parse' = parse

derive instance Generic PError _

instance Show PError where
  show = genericShow

derive instance Generic (Threeple a b c) _

instance (Show a, Show b, Show c) ⇒ Show (Threeple a b c) where
  show = genericShow

char ∷ ∀ e. Parser e Char
char = Parser \s → case uncons s of
  Nothing → Left eof
  Just { head, tail } → Right $ Tuple tail head

-- 4. These are our applicative parsers. Rewrite them using our new monadic parser using do notation.
--    Replace in new function name A with B, e.g. twoCharsA → twoCharsB.

twoCharsA ∷ ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

twoCharsB ∷ ∀ e. Parser e (Tuple Char Char)
twoCharsB = do
  c1 ← char
  c2 ← char
  pure $ Tuple c1 c2

threeCharsA ∷ ∀ e. Parser e (Tuple Char (Tuple Char Char))
threeCharsA = Tuple <$> char <*> twoCharsA

threeCharsB ∷ ∀ e. Parser e (Tuple Char (Tuple Char Char))
threeCharsB = do
  c1 ← char
  c2 ← twoCharsB
  pure $ Tuple c1 c2

threeCharsA' ∷ ∀ e. Parser e (Threeple Char Char Char)
threeCharsA' = Threeple <$> char <*> char <*> char

threeCharsB' ∷ ∀ e. Parser e (Threeple Char Char Char)
threeCharsB' = do
  c1 ← char
  c2 ← char
  c3 ← char
  pure $ Threeple c1 c2 c3

threeCharsA'' ∷ ∀ e. Parser e String
threeCharsA'' = (\c1 c2 c3 → fromCharArray [ c1, c2, c3 ]) <$> char <*> char <*> char

threeCharsB'' ∷ ∀ e. Parser e String
threeCharsB'' = do
  c1 ← char
  c2 ← char
  c3 ← char
  pure $ fromCharArray [ c1, c2, c3 ]

tenCharsA ∷ ∀ e. Parser e String
tenCharsA = (\c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 → fromCharArray [ c1, c2, c3, c4, c5, c6, c7, c8, c9, c10 ])
  <$> char
  <*> char
  <*> char
  <*> char
  <*> char
  <*> char
  <*> char
  <*> char
  <*> char
  <*> char

tenCharsB ∷ ∀ e. Parser e String
tenCharsB = do
  c1 ← char
  c2 ← char
  c3 ← char
  c4 ← char
  c5 ← char
  c6 ← char
  c7 ← char
  c8 ← char
  c9 ← char
  c10 ← char
  pure $ fromCharArray [ c1, c2, c3, c4, c5, c6, c7, c8, c9, c10 ]

-- 5. Write a parser that always fails.
fail ∷ ∀ e a. ParserError e ⇒ e → Parser e a
fail err = Parser $ const (Left err)

-- 6. Write a satisfy function (the first argument - the String - is an error message).
satisfy ∷ ∀ e. ParserError e ⇒ String → (Char → Boolean) → Parser e Char
satisfy errMsg p = do
  c ← char
  if p c then pure c else fail (invalidChar errMsg)

-- 7. Write a Char-parser called digit parser based on satisfy using isDecDigit.
digit ∷ ∀ e. ParserError e ⇒ Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

-- 8. Write a Char-parser called letter parser (use isAlpha).
letter ∷ ∀ e. ParserError e ⇒ Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

-- 9. Write an alphanum parser using the digit and letter parsers. Make sure it provides a meaningful error message.
alphaNum ∷ ∀ e. ParserError e ⇒ Parser e Char
alphaNum = digit <|> letter <|> fail (invalidChar "alphaNum")

count ∷ ∀ m f a. Applicative m ⇒ Unfoldable f ⇒ Traversable f ⇒ Int → m a → m (f a)
count = replicateA

-- 10. Refactor count to make it more pleasant to work with and call it count'.
count' ∷ ∀ e. ParserError e ⇒ Int → Parser e Char → Parser e String
count' n p = fromCharArray <$> count n p

main ∷ Effect Unit
main = do
  log "Exercisse Chapter 19 - Monadic Parser."
  log "-------------------------"
  log "-- Applicative Parsers --"
  log "-------------------------"
  log $ show $ (parse char "ABC" ∷ Either PError _) ------------------------------------ (Right (Tuple "BC" 'A')).
  log $ show $ (parse twoCharsA "ABC" ∷ Either PError _) ------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ (parse threeCharsA "ABC" ∷ Either PError _) ----------------------------- (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ (parse threeCharsA' "ABC" ∷ Either PError _) ---------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ (parse threeCharsA'' "ABC" ∷ Either PError _) --------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' char "ABC" ------------------------------------------------------- (Right (Tuple "BC" 'A')).
  log $ show $ parse' twoCharsA "ABC" -------------------------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ parse' threeCharsA "ABC" ------------------------------------------------ (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ parse' threeCharsA' "ABC" ----------------------------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ parse' threeCharsA'' "ABC" ---------------------------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' threeCharsA "A" -------------------------------------------------- (Left EOF)
  log $ show $ parse' tenCharsA "ABCDEFGHIJKLMNOPQRSTUVXYZ" ---------------------------- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log "---------------------"
  log "-- Monadic Parsers --"
  log "---------------------"
  log $ show $ (parse char "ABC" ∷ Either PError _) ------------------------------------ (Right (Tuple "BC" 'A')).
  log $ show $ (parse twoCharsB "ABC" ∷ Either PError _) ------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ (parse threeCharsB "ABC" ∷ Either PError _) ----------------------------- (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ (parse threeCharsB' "ABC" ∷ Either PError _) ---------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ (parse threeCharsB'' "ABC" ∷ Either PError _) --------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' char "ABC" ------------------------------------------------------- (Right (Tuple "BC" 'A')).
  log $ show $ parse' twoCharsB "ABC" -------------------------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ parse' threeCharsB "ABC" ------------------------------------------------ (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ parse' threeCharsB' "ABC" ----------------------------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ parse' threeCharsB'' "ABC" ---------------------------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' threeCharsB "A" -------------------------------------------------- (Left EOF)
  log $ show $ parse' tenCharsB "ABCDEFGHIJKLMNOPQRSTUVXYZ" ---------------------------- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log "----------------------"
  log "-- Helper Functions --"
  log "----------------------"
  log $ show $ parse' (fromCharArray <$> (count 10 char)) "ABCDEFGHIJKLMNOPQRSTUVXYZ" -- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log $ show $ parse' (count' 3 digit) "123456" ---------------------------------------- (Right (Tuple "456" "123"))
  log $ show $ parse' (count' 3 digit) "abc456" ---------------------------------------- (Left (InvalidChar "digit"))
  log $ show $ parse' (count' 4 letter) "Freddy" --------------------------------------- (Right (Tuple "dy" "Fred"))
  log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5" -------------------------------- (Right (Tuple "" "a1b2c3d4e5"))
  log $ show $ parse' (count' 10 alphaNum) "######" ------------------------------------ (Left (InvalidChar "alphaNum"))
