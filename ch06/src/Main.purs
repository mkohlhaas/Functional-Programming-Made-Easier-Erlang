module Main where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (CodePoint)
import Data.String as StringUnicode
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

newtype LastName = LastName String
newtype FirstName = FirstName String

derive instance Newtype LastName _
derive instance Newtype FirstName _

fullName :: FirstName -> LastName -> String
fullName (FirstName first) (LastName last) = first <> " " <> last

fullName' :: FirstName -> LastName -> String
fullName' first last = unwrap first <> " " <> unwrap last

glueNames :: ∀ a b. Newtype a String => Newtype b String => String -> a -> b -> String
glueNames between n1 n2 = unwrap n1 <> between <> unwrap n2

lastNameFirst :: LastName -> FirstName -> String
lastNameFirst = glueNames ", "

fullName'' :: FirstName -> LastName -> String
fullName'' = glueNames " "

------------------------------------------------------------------------------------------

class Decapitate collection element where
  decapitate :: collection -> Maybe { head :: element, tail :: collection }

instance decapitateList :: Decapitate (List a) a where
  decapitate = List.uncons

instance decapitateString :: Decapitate String Char where
  decapitate = String.uncons

instance decapitateStringUnicode :: Decapitate String CodePoint where
  decapitate = StringUnicode.uncons

genericTail :: ∀ collection element. Decapitate collection element => Proxy element -> collection -> Maybe collection
genericTail _ xs = case decapitate xs :: Maybe { head :: element, tail :: collection } of
  Just { tail } -> Just tail
  Nothing -> Nothing

t :: Maybe String
t = genericTail (Proxy :: Proxy Char) "ab🍝c∀"

tu :: Maybe String
tu = genericTail (Proxy :: Proxy CodePoint) "ab🍝c∀"

main :: Effect Unit
main = do
  log $ fullName (FirstName "Hans") (LastName "Schmid")
  log $ fullName' (FirstName "Hans") (LastName "Schmid")
  log $ fullName'' (FirstName "Hans") (LastName "Schmid")
  log $ lastNameFirst (LastName "Schmid") (FirstName "Hans")
  log $ fromMaybe "" t
  log $ fromMaybe "" tu
