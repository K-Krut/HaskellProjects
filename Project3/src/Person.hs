module Person where

import Data.List
import Data.Time
import Data.Maybe
import Date

type Name = String
type FamilyName = String
type Company = String
type Telephone = String
type Mail = String
type Birthday = Maybe Date

type Group = String


data Person = Person {
  name :: Name, 
  familyName :: FamilyName,
  company :: Company,
  telephone :: Telephone, 
  mail :: Mail,
  birthday :: Birthday, 
  groups :: [Group]}  deriving (Show, Read)
  

instance Ord Person where
  compare a b 
    | a == b				= EQ
    | familyName a /= familyName b 	= compare (familyName a) (familyName b)
    | name a /= name b			= compare (name a) (name b)
    | birthday a /= birthday b		= compare (birthday a) (birthday b)
    | mail a /= mail b			= compare (mail a) (mail b)
    | otherwise 			= EQ
    

instance Eq Person where
  (==) a b = (name a == name b) && (familyName a == familyName b) && (telephone a == telephone b)
 -- (==) a b = mail a == mail b


printablePerson :: Person -> String
printablePerson p = concat $ intersperse " " [name p, familyName p, company p, telephone p, mail p, printableDate (birthday p), printableGroups p]


printableGroups :: Person -> String
printableGroups p = concat $ intersperse ", " (groups p)


hasBirthday :: Date -> Person  -> Bool
hasBirthday (Date date) person
  | isNothing $ birthday person 	= False
  | otherwise 				= isAnniversary (fromJust $ birthday person) day month where
  (_, m, d) = toGregorian date
  month = toInteger m
  day = toInteger d
  


joinGroup :: Group -> Person -> Person
joinGroup g p 
  | not (g `elem` (groups p)) = p { groups = newGroupList }
  | otherwise = p 
  where
   newGroupList = insert g (groups p)
      


leaveGroup :: Group -> Person -> Person
leaveGroup g p = p { groups = newGroupList } where
  newGroupList = delete g (groups p)
  

changeGroup :: Group -> Group -> Person -> Person
changeGroup old new person 
  | old `elem` gp	= person { groups = 
    if not $ new `elem` gp
	 then changed
	 else oldDeleted
  }
  | otherwise = person
    where
      changed = insert new oldDeleted
      oldDeleted = delete old gp
      gp = groups person
      
