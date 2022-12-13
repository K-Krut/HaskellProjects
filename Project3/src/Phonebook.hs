module Phonebook where

import Data.List
import Data.Time 
import Data.Maybe

import Person
import Meeting
import Date


data Phonebook = Phonebook [Person] [Group] [Meeting] deriving (Show, Read)


addPerson :: Phonebook -> Person -> Phonebook
addPerson (Phonebook pList gList mList) person = Phonebook (insert person pList) gList mList

addMeetingData :: Phonebook -> Meeting -> Phonebook
addMeetingData (Phonebook pList gList mList) meeting = Phonebook pList gList (insert meeting mList)

removePerson :: Phonebook -> Person -> Phonebook
removePerson (Phonebook pList gList mList) person = Phonebook (delete person pList) gList mList

removeMeeting :: Phonebook -> Meeting -> Phonebook
removeMeeting (Phonebook pList gList mList) meeting = Phonebook pList gList (delete meeting  mList)

editPerson :: Phonebook -> Person -> Person -> Phonebook 
editPerson book@(Phonebook pList gList mList) old new = Phonebook (replace pList old new) gList mList

editMeeting :: Phonebook -> Meeting -> Meeting -> Phonebook
editMeeting book@(Phonebook pList gList mList) old new = Phonebook pList gList (replace mList old new)

findPeopleBy :: (Person -> String) -> String -> Phonebook -> [Person]
findPeopleBy f value (Phonebook pList _ _) = filter ((isPrefixOf value) . f) pList

findMeetingBy :: (Meeting -> String) -> String -> Phonebook -> [Meeting]
findMeetingBy f value (Phonebook _ _ mList) = filter ((isPrefixOf value) . f) mList


findBirthdayPeople :: Phonebook -> IO [Person]
findBirthdayPeople (Phonebook pList _ _) = do
  c <- getCurrentTime
  let today = Date $ utctDay c
  return (filter (hasBirthday today) pList)


findPeopleInGroup :: Phonebook -> Group -> [Person]
findPeopleInGroup (Phonebook pList gList _) g = filter (\x -> g `elem` (groups x)) pList


addGroup :: Phonebook -> Group -> Phonebook
addGroup book@(Phonebook pList gList mList) newGroup
  | not $ newGroup `elem` gList = (Phonebook pList newGroupList mList)
  | otherwise 			 = book
  where
    newGroupList = insert newGroup gList


deleteGroup :: Phonebook -> Group -> Phonebook
deleteGroup (Phonebook pList gList mList) g = (Phonebook newPersonList newGroupList mList) where
  newPersonList = map (leaveGroup g) pList
  newGroupList = delete g gList


renameGroup :: Phonebook -> Group -> Group -> Phonebook
renameGroup book@(Phonebook pList gList mList) old new = (Phonebook newPersonList newGroupList mList) where
  newGroupList = if not $ new `elem` gList
		    then sort $ new : afterDeletion
		    else afterDeletion
  afterDeletion = delete old gList
  newPersonList =  map (changeGroup old new) pList 
  

mergeGroups :: Phonebook -> Group -> Group -> Phonebook
mergeGroups book old new = renameGroup book old new


addPersonToGroup :: Phonebook -> Person -> Group -> Phonebook
addPersonToGroup book@(Phonebook pList gList mList) p g = editPerson book p (joinGroup g p)


removePersonFromGroup :: Phonebook -> Person -> Group -> Phonebook
removePersonFromGroup book@(Phonebook pList gList mList) p g = editPerson book p (leaveGroup g p)


deleteAll :: (Eq a) => a -> [a] -> [a]
deleteAll d list = [x | x <- list, x /= d]


replace :: (Eq a, Ord a) => [a] -> a -> a -> [a]
replace list old new 
  | old `elem` list  = if (compare old new) == EQ
			  then result
			  else sort result
  | otherwise = error "Próba wymiany nieistniejącego elementu listy"
  where
    result = beginning ++ new : ending
    beginning = take position list
    ending = drop (position + 1) list
    position = fromJust $ elemIndex old list 


