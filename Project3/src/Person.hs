module Person where

import Data.List
import Data.Time
import Data.Maybe
import Date

type Name = String
type FamilyName = String
type Telephone = String
type Birthday = Maybe Date



data Person = Person {
  name :: Name, 
  familyName :: FamilyName,
  telephone :: Telephone,
  birthday :: Birthday
  }  deriving (Show, Read)
  

instance Ord Person where
  compare a b 
    | a == b				= EQ
    | familyName a /= familyName b 	= compare (familyName a) (familyName b)
    | name a /= name b			= compare (name a) (name b)
    | birthday a /= birthday b		= compare (birthday a) (birthday b)
    | otherwise 			= EQ
    

instance Eq Person where
  (==) a b = (name a == name b) && (familyName a == familyName b) && (telephone a == telephone b)


printablePerson :: Person -> String
printablePerson p = concat $ intersperse " " [name p, familyName p, telephone p, printableDate (birthday p)]


hasBirthday :: Date -> Person  -> Bool
hasBirthday (Date date) person
  | isNothing $ birthday person 	= False
  | otherwise 				= isAnniversary (fromJust $ birthday person) day month where
  (_, m, d) = toGregorian date
  month = toInteger m
  day = toInteger d
  

      
