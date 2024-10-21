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

derive instance Functor Maybe

instance Apply Maybe where
  apply Nothing _ = Nothing
  apply (Just f) x = f <$> x

instance Applicative Maybe where
  pure = Just

------------
-- Either --
------------

data Either a b = Left a | Right b

derive instance (Eq a, Eq b) ⇒ Eq (Either a b)

derive instance Generic (Either a b) _

instance (Show a, Show b) ⇒ Show (Either a b) where
  show = genericShow

derive instance Functor (Either a)

instance Apply (Either a) where
  apply (Left x) _ = Left x
  apply (Right f) x = f <$> x

instance Applicative (Either a) where
  pure = Right

----------------
-- Validation --
----------------

newtype Validation err result = Validation (Either err result)

type FamilyAgesRow r = (fatherAge ∷ Age, motherAge ∷ Age, childAge ∷ Age | r)
newtype FamilyAges = FamilyAges { | FamilyAgesRow () }
newtype Age = Age Int
newtype LowerAge = LowerAge Int
newtype UpperAge = UpperAge Int
data FamilyMember = Father | Mother | Child

derive newtype instance Show Age
derive newtype instance Show FamilyAges
derive newtype instance (Show err, Show result) ⇒ Show (Validation err result)

derive instance Generic FamilyMember _
instance Show FamilyMember where
  show = genericShow

derive instance Functor (Validation err)

instance Semigroup err ⇒ Apply (Validation err) where
  apply (Validation (Left x)) (Validation (Left y)) = Validation $ Left (x <> y)
  apply (Validation (Left x)) _ = Validation $ Left x
  apply (Validation (Right f)) x = f <$> x

-- Write `validateAge` function
validateAge ∷ LowerAge → UpperAge → FamilyMember → Age → Validation (Array String) Age
validateAge (LowerAge la) (UpperAge ua) fm age@(Age a)
  | a < la = Validation $ Left [ show fm <> " is too young." ]
  | a > ua = Validation $ Left [ show fm <> " is too old." ]
  | otherwise = Validation $ Right age

validateAgeFather ∷ Age → Validation (Array String) Age
validateAgeFather = validateAge (LowerAge 18) (UpperAge 99) Father

validateAgeMother ∷ Age → Validation (Array String) Age
validateAgeMother = validateAge (LowerAge 18) (UpperAge 99) Mother

validateAgeChild ∷ Age → Validation (Array String) Age
validateAgeChild = validateAge (LowerAge 3) (UpperAge 99) Child

createFamilyAges ∷ { | FamilyAgesRow () } → Validation (Array String) FamilyAges
createFamilyAges { fatherAge, motherAge, childAge } = FamilyAges <$> ({ fatherAge: _, motherAge: _, childAge: _ } <$> validateAgeFather fatherAge <*> validateAgeMother motherAge <*> validateAgeChild childAge)

----------
-- Main --
----------
main ∷ Effect Unit
main = do
  log "Exercise Chapter 17."
  log "No further imports allowed!!!"
  log "------------------------------------"
  log "-- Applicative Instance for Maybe --"
  log "------------------------------------"
  log $ show $ (+) <$> Just 21 <*> Just 21 -------------- (Just 42)
  log $ show $ (*) <$> pure 2 <*> (pure 21 ∷ Maybe _) --- (Just 42)
  log $ show $ pure (+) <*> Just 17 <*> Just 25 --------- (Just 42)
  log "-------------------------------------"
  log "-- Applicative Instance for Either --"
  log "-------------------------------------"
  log "Associative Composition Law: (<<<) <$> u <*> v <*> w = u <*> (v <*> w)"
  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> (pure 1 ∷ Either Unit _)))
  log "Identity Law: pure identity <*> x = x"
  log $ show $ (pure identity <*> pure 1) == (pure 1 ∷ Either Unit _)
  log "Homomorphism Law: pure (f x) = pure f <*> pure x"
  log $ show $ (pure (negate 1)) == (pure negate <*> (pure 1 ∷ Either Unit _))
  log "Interchange Law: u <*> pure x = pure (_ $ x) <*> u"
  log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> (pure negate ∷ Either Unit _))
  log "----------------"
  log "-- Validation --"
  log "----------------"
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 30, childAge: Age 10 } --- (Validation (Right (FamilyAges { childAge: 10, fatherAge: 40, motherAge: 30 })))
  log $ show $ createFamilyAges { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0 } -- (Validation (Left ["Father is too old.", "Mother is too old.", "Child is too young."]))
  log $ show $ createFamilyAges { fatherAge: Age 4, motherAge: Age 3, childAge: Age 10 } ----- (Validation (Left ["Father is too young.", "Mother is too young."]))
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 30, childAge: Age 100 } -- (Validation (Left ["Child is too old."]))
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 3, childAge: Age 0 } ----- (Validation (Left ["Mother is too young.", "Child is too young."]))
