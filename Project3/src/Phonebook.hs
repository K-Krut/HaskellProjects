module Phonebook where

import Data.List
import Data.Time 
import Data.Maybe

import Person
import Meeting
import Date


data Phonebook = Phonebook [Person] [Meeting] deriving (Show, Read)


addPerson :: Phonebook -> Person -> Phonebook
addPerson (Phonebook pList mList) person = Phonebook (insert person pList) mList

addMeetingData :: Phonebook -> Meeting -> Phonebook
addMeetingData (Phonebook pList mList) meeting = Phonebook pList (insert meeting mList)

removePerson :: Phonebook -> Person -> Phonebook
removePerson (Phonebook pList mList) person = Phonebook (delete person pList) mList

removeMeeting :: Phonebook -> Meeting -> Phonebook
removeMeeting (Phonebook pList mList) meeting = Phonebook pList (delete meeting mList)

editPerson :: Phonebook -> Person -> Person -> Phonebook 
editPerson book@(Phonebook pList mList) old new = Phonebook (replace pList old new) mList

editMeeting :: Phonebook -> Meeting -> Meeting -> Phonebook
editMeeting book@(Phonebook pList mList) old new = Phonebook pList (replace mList old new)

findPeopleBy :: (Person -> String) -> String -> Phonebook -> [Person]
findPeopleBy f value (Phonebook pList _) = filter ((isPrefixOf value) . f) pList

findMeetingBy :: (Meeting -> String) -> String -> Phonebook -> [Meeting]
findMeetingBy f value (Phonebook _ mList) = filter ((isPrefixOf value) . f) mList


findBirthdayPeople :: Phonebook -> IO [Person]
findBirthdayPeople (Phonebook pList _) = do
  c <- getCurrentTime
  let today = Date $ utctDay c
  return (filter (hasBirthday today) pList)



deleteAll :: (Eq a) => a -> [a] -> [a]
deleteAll d list = [x | x <- list, x /= d]


replace :: (Eq a, Ord a) => [a] -> a -> a -> [a]
replace list old new 
  | old `elem` list  = if (compare old new) == EQ
			  then result
			  else sort result
  | otherwise = error "Елемента не існує"
  where
    result = beginning ++ new : ending
    beginning = take position list
    ending = drop (position + 1) list
    position = fromJust $ elemIndex old list 


