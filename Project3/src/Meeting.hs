module Meeting where

import Data.List
import Data.Time
import Data.Maybe
import Date

type NamePlace = String
type Place = String
type DateMeeting = Maybe Date
type HeldMeeting = String

data Meeting = Meeting {
  namePlace :: NamePlace,
  place :: Place,
  dateMeeting :: DateMeeting,
  held :: HeldMeeting
}  deriving (Show, Read)


instance Ord Meeting where
  compare a b
    | a == b				                    = EQ
    | held a /= held b	              	= compare (held a) (held b)
    | dateMeeting a /= dateMeeting b		= compare (dateMeeting a) (dateMeeting b)
    | namePlace a /= namePlace b		    = compare (namePlace a) (namePlace b)
    | place a /= place b		          	= compare (place a) (place b)
    | otherwise 			                = EQ




instance Eq Meeting where
  (==) a b = (dateMeeting a == dateMeeting b) && (place a == place b) && (namePlace a == namePlace b) && (held a == held b)


printableMeeting :: Meeting -> String
printableMeeting p = concat $ intersperse " " [namePlace p, place p, printableDate (dateMeeting p), held p]

