module Main where

import Prelude

import Data.Array (replicate)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicateA)
import Effect (Effect)
import Effect.Console (log)

-- The Parsing State needs to be passed from Parser to Parser, i.e. when the current Parser is done,
-- it passes what's left of the String to the next Parser. Also, if a single Parser in the chain were to fail,
-- we want to short-circuit the parsing and return the error.

---------------------------------
-- Data Types and Type Classes --
---------------------------------

-- e = error type
-- a = return type

class ParserError e where
  eof ∷ e

data PError = EOF -- application specific parse error type
type ParserState a = Tuple String a -- left-over string and parsed value
type ParseFunction e a = ParserError e ⇒ String → Either e (ParserState a)
newtype Parser e a = Parser (ParseFunction e a)
data Threeple a b c = Threeple a b c

-------------------------------------------
-- Create Necessary Instances for Parser --
-------------------------------------------

derive instance Functor (Parser e)

-- instance Apply (Parser e) where
--   apply (Parser p1) (Parser p2) =
--     Parser $ \str → case p1 str of
--       Left err → Left err
--       Right (Tuple str' f) → case p2 str' of
--         Left err' → Left err'
--         Right (Tuple str'' a) → Right (Tuple str'' (f a))

instance Applicative (Parser e) where
  pure a = Parser $ \str → Right (Tuple str a)

----------------------
-- Using the Parser --
----------------------

-- Write a parse function
parse ∷ ∀ e a. Parser e a → ParseFunction e a
parse (Parser p) = p

-- Use parse in map/apply
instance Apply (Parser e) where
  apply p1 p2 =
    Parser $ \str → case parse p1 str of
      Left err → Left err
      Right (Tuple str' f) → case parse p2 str' of
        Left err' → Left err'
        Right (Tuple str'' a) → Right (Tuple str'' (f a))

-- Create Show instance for PError
derive instance Generic PError _
instance Show PError where
  show = genericShow

-- Create ParserError instance for PError
instance ParserError PError where
  eof = EOF

-- Write a char parser using String libary function uncons
-- char ∷ ∀ e. Parser e Char
char ∷ ∀ e. Parser e Char
char = Parser $ \str → case uncons str of
  Nothing → Left eof
  Just { head, tail } → Right $ Tuple tail head

-- Write a two-char parser
twoChars ∷ ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char

-- Write a char-two-char parser
threeChars ∷ ∀ e. Parser e (Tuple Char (Tuple Char Char))
threeChars = Tuple <$> char <*> twoChars

-- Write a Show instance for Threeple
derive instance Generic (Threeple a b c) _
instance (Show a, Show b, Show c) ⇒ Show (Threeple a b c) where
  show = genericShow

-- Write a Threeple 3-char parser
threeChars' ∷ ∀ e. Parser e (Threeple Char Char Char)
threeChars' = Threeple <$> char <*> char <*> char

-- Write a 3-char parser returning a String using library function fromCharArray
threeChars'' ∷ ∀ e. Parser e String
threeChars'' = (\c1 c2 c3 → fromCharArray [ c1, c2, c3 ]) <$> char <*> char <*> char

-- Write a parse' function which includes the error type in its signature
parse' ∷ ∀ a. Parser PError a → ParseFunction PError a
parse' = parse

-- Write a 10-char parser in the same manner as the 3-char parser
tenChars ∷ ∀ e. Parser e String
tenChars = (\c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 → fromCharArray [ c1, c2, c3, c4, c5, c6, c7, c8, c9, c10 ]) <$> char <*> char <*> char <*> char <*> char <*> char <*> char <*> char <*> char <*> char

-- Do the same using sequence from Data.Traversable and replicate from Data.Array using this helper function
count ∷ ∀ e a. Int → Parser e a → Parser e (Array a)
count n p = sequence $ replicate n p

-- Make count more generic and call it count'
count' ∷ ∀ m f a. Applicative m ⇒ Unfoldable f ⇒ Traversable f ⇒ Int → m a → m (f a)
count' = replicateA

main ∷ Effect Unit
main = do
  log "Exercise Ch. 17 Applicative Parser."
  log $ show $ (parse char "ABC" ∷ Either PError _) ----------------------------------------- (Right (Tuple "BC" 'A')).
  log $ show $ (parse twoChars "ABC" ∷ Either PError _) ------------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ (parse threeChars "ABC" ∷ Either PError _) ----------------------------------- (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ (parse threeChars' "ABC" ∷ Either PError _) ---------------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ (parse threeChars'' "ABC" ∷ Either PError _) --------------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' char "ABC" ------------------------------------------------------------ (Right (Tuple "BC" 'A')).
  log $ show $ parse' twoChars "ABC" -------------------------------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ parse' threeChars "ABC" ------------------------------------------------------ (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ parse' threeChars' "ABC" ----------------------------------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ parse' threeChars'' "ABC" ---------------------------------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' threeChars "A" -------------------------------------------------------- (Left EOF)
  log $ show $ parse' tenChars "ABCDEFGHIJKLMNOPQRSTUVXYZ" ---------------------------------- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log $ show $ parse' (fromCharArray <$> (count 10 char)) "ABCDEFGHIJKLMNOPQRSTUVXYZ" ------- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log $ show $ parse' (fromCharArray <$> (count' 10 char)) "ABCDEFGHIJKLMNOPQRSTUVXYZ" ------ (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log $ show $ parse' (fromCharArray <$> (count' (-10) char)) "ABCDEFGHIJKLMNOPQRSTUVXYZ" --- (Right (Tuple "ABCDEFGHIJKLMNOPQRSTUVXYZ" ""))
