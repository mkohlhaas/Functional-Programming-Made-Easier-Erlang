module Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Array ((:))
import Control.Lazy (class Lazy, defer)
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicateA)

---------------------------------
-- Data Types and Type Classes --
---------------------------------

class ParserError e where
  eof ∷ e
  invalidChar ∷ String → e

data PError = EOF | InvalidChar String
type ParserState a = Tuple String a
type ParseFunction e a = ParserError e ⇒ String → Either e (ParserState a)
newtype Parser e a = Parser (ParseFunction e a)

data Threeple a b c = Threeple a b c

derive instance Generic (Threeple a b c) _

instance (Show a, Show b, Show c) ⇒ Show (Threeple a b c) where
  show = genericShow

derive instance Eq PError
derive instance Generic PError _

instance Show PError where
  show = genericShow

instance ParserError PError where
  eof = EOF
  invalidChar = InvalidChar

instance Functor (Parser e) where
  map f x = Parser \s → map f <$> parse x s

instance Apply (Parser e) where
  apply = ap

instance Applicative (Parser e) where
  pure x = Parser \s → Right $ Tuple s x

instance Bind (Parser e) where
  bind x f = Parser \str → do
    Tuple str' x' ← parse x str
    parse (f x') str'

instance Monad (Parser e)

instance Alt (Parser e) where
  alt p1 p2 = Parser \str → parse p1 str <|> parse p2 str

instance Lazy (Parser e a) where
  defer f = Parser $ \str → parse (f unit) str

------------------------
-- Parser Combinators --
------------------------

parse ∷ ∀ e a. Parser e a → ParseFunction e a
parse (Parser f) = f

parse' ∷ ∀ a. Parser PError a → ParseFunction PError a
parse' = parse

char ∷ ∀ e. Parser e Char
char = Parser \s → case uncons s of
  Nothing → Left eof
  Just { head, tail } → Right $ Tuple tail head

count ∷ ∀ e a f. Traversable f ⇒ Unfoldable f ⇒ Int → Parser e a → Parser e (f a)
count = replicateA

count' ∷ ∀ e. Int → Parser e Char → Parser e String
count' n p = fromCharArray <$> count n p

fail ∷ ∀ e a. ParserError e ⇒ e → Parser e a
fail err = Parser $ const $ Left err

satisfy ∷ ∀ e. ParserError e ⇒ String → (Char → Boolean) → Parser e Char
satisfy errMsg p = char >>= \c → if p c then pure c else fail (invalidChar errMsg)

digit ∷ ∀ e. ParserError e ⇒ Parser e Char
digit = satisfy "digit" $ isDecDigit <<< codePointFromChar

letter ∷ ∀ e. ParserError e ⇒ Parser e Char
letter = satisfy "letter" $ isAlpha <<< codePointFromChar

alphaNum ∷ ∀ e. ParserError e ⇒ Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

optional ∷ ∀ e a. a → Parser e a → Parser e a
optional def p = p <|> pure def

atMost' ∷ ∀ e. Int → Parser e Char → Parser e String
atMost' n p = fromCharArray <$> atMost (:) n p

atMost ∷ ∀ e f a. Unfoldable f ⇒ (a → f a → f a) → Int → Parser e a → Parser e (f a)
atMost _ n _ | n <= 0 = pure none
atMost cons n p = optional none (p >>= \x → cons x <$> atMost cons (n - 1) p)

range' ∷ ∀ e. ParserError e ⇒ Int → Int → Parser e Char → Parser e String
range' min max p = fromCharArray <$> range (:) min max p

range ∷ ∀ e f a. Semigroup (f a) ⇒ Traversable f ⇒ Unfoldable f ⇒ (a → f a → f a) → Int → Int → Parser e a → Parser e (f a)
range cons min max p | min <= max = (<>) <$> count min p <*> atMost cons (max - min) p
range _ _ _ _ = pure none

constChar ∷ ∀ e. ParserError e ⇒ Char → Parser e Unit
constChar = void <<< constChar'

constChar' ∷ ∀ e. ParserError e ⇒ Char → Parser e Char
constChar' c = satisfy (show c) (_ == c)
