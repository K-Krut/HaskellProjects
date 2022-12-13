module Interface (addContact,
      addMeeting,
		  printContactsFile,
		  printMeetingsFile,
		  printGroup,
		  find,
		  findM,
		  pressEnter', 
		  editOrRemoveP,
		  editOrRemoveMeeting,
		  newGroup, 
		  printAllGroups,
		  addPerToGr,
		  removePerFromGr,
		  groupChangeName,
		  sumGroups,
		  removeGroup,
		  whoseBirthday) where


import Phonebook
import Person
import Meeting
import Terminal
import Date
import DataStorage
import Data.Char
import Data.Maybe



dataFile = "contacts"




getBook = DataStorage.loadBook dataFile

saveNewBook newBook = DataStorage.overwriteBook newBook dataFile

getPerson book num = (getPList book) !! (num - 1)

getMeeting book num = (getMList book) !! (num - 1)

getPList (Phonebook pList gList mList) = pList

getMList (Phonebook pList gList mList) = mList

getGList :: Phonebook -> [Group]
getGList (Phonebook pList gList mList) = gList



addContact :: IO ()
addContact = do book <- getBook
		personToAdd <- getPersonData
		if not (personToAdd `elem` (getPList book))
		  then saveNewBook $ addPerson book personToAdd
		else putStrFlush "Такий контакт уже існує"


getPersonData = do
		name <- promptString' "Імʼя" validName
	        familyName <- promptString' "Фамілія" validName
		telephone  <- promptString' "# Телефону" validPhone
		birthday  <- promptString' "Дата народження (dd.mm.rrrr)" validDate
		return $ Person name familyName telephone (stringToDate birthday) []


addMeeting :: IO ()
addMeeting = do book <- getBook
		meetingToAdd <- getMeetingData
		if not (meetingToAdd `elem` (getMList book))
		  then saveNewBook $ addMeetingData book meetingToAdd
		else putStrFlush "Така зустріч вже існує"


getMeetingData = do
		namePlace <- promptString' "Імʼя" validName
	        place <- promptString' "Місце" validName
		dateMeeting <- promptString' "Дата (dd.mm.rrrr)" validDate
		held <- promptLine "Held?"
		return $ Meeting namePlace place (stringToDate dateMeeting) held



validName :: String -> Bool
validName x = (x /= "") && ( and $ map (\c -> isLetter c || c == ' ') x)


validPhone :: String -> Bool
validPhone x = and $ map isDigit x


validMail :: String -> Bool
validMail x = '@' `elem` x && '.' `elem` x


validDate :: String -> Bool
validDate x = x == "" || (isJust $ stringToDate x)


printGroup = do book <- getBook
		groupName <- promptLine "Назва групи"
		showBook "Контакти в групі" $ (Phonebook (findPeopleInGroup book groupName) [] [])
		pressEnter


printContactsFile = getBook >>= showBook "Контакти"  >> pressEnter

printMeetingsFile = getBook >>= showBookM "Зустрічі"  >> pressEnter


find byWhat functionAtEnd= do book <- getBook
			      value <- promptLine "Префікс:"
			      showBook "Результати" (Phonebook (findPeopleBy byWhat value book) [] [])
			      functionAtEnd (Phonebook (findPeopleBy byWhat value book) [] [])


findM byWhat functionAtEnd = do
      book <- getBook
      value <- promptLine "Префікс:"
      showBook "Результати" (Phonebook [] [] (findMeetingBy byWhat value book))
      functionAtEnd (Phonebook [] [] (findMeetingBy byWhat value book))



pressEnter':: Phonebook -> IO ()
pressEnter' whatever = promptLine "ENTER.." >> return ()


whoFromResultsToEdit matchingGuysBook = if (inputLength > 1)
						then do nr <- prompt' "Введи номер"  (\c -> c >= 1 && c <= inputLength)
							return $ getPerson matchingGuysBook nr
						else return $ getPerson matchingGuysBook 1
							where inputLength = length (getPList matchingGuysBook)


whoFromResultsToEditM matchingMeetings = if (inputLength > 1)
						then do nr <- prompt' "Номер зустрічі"  (\c -> c >= 1 && c <= inputLength)
							return $ getMeeting matchingMeetings nr
						else return $ getMeeting matchingMeetings 1
							where inputLength = length (getMList matchingMeetings)


editOrRemoveP matchingGuysBook = do persona <- whoFromResultsToEdit matchingGuysBook
				    editOrDelete <- prompt' "Опції:\n 1) Редагувати\n 2) Видалити\n 3) <---\n "  (\c -> c `elem` [1,2,3])
				    resultAction editOrDelete persona
					where resultAction x persona = case x of
								    1 -> editContact persona
								    2 -> deleteContact persona
								    3 -> return ()


editOrRemoveMeeting matchingMeetings = do
      meeting <- whoFromResultsToEditM matchingMeetings
      editOrDelete <- prompt' "Опції:\n 1) Редагувати\n 2) Видалити\n 3) <---\n "  (\c -> c `elem` [1,2])
      resultAction editOrDelete meeting
        where resultAction x meeting = case x of
                1 -> Interface.editMeeting meeting
                2 -> deleteMeeting meeting
                3 -> return ()



editContact :: Person -> IO ()
editContact oldPerson = do book <- getBook
			   putStrFlush "Введіть нові дані:\n"
			   newPerson <- getPersonData
			   saveNewBook $ editPerson book oldPerson newPerson
			   putStrFlush "---------------> Контакт змінено\n" >> pressEnter


editMeeting :: Meeting -> IO ()
editMeeting oldM = do
      book <- getBook
      putStrFlush "Podaj nowe dane kontaktu:\n"
      newM <- getMeetingData
      saveNewBook $ Phonebook.editMeeting book oldM newM
      putStrFlush "---------------> Контакт змінено\n" >> pressEnter


deleteContact :: Person -> IO ()
deleteContact personToDel = do book <- getBook
			       saveNewBook $ removePerson book personToDel
			       putStrFlush "---------------> Контакт змінено\n" >> pressEnter


deleteMeeting :: Meeting -> IO ()
deleteMeeting meetingToDel = do
              book <- getBook
              saveNewBook $ removeMeeting book meetingToDel
              putStrFlush "---------------> Контакт змінено\n" >> pressEnter


newGroup = do book <- getBook
	      group <- promptLine "Введіть назву групи"
	      saveNewBook $ addGroup book group


printAllGroups = do book <- getBook
		    showItems "Існуючі групи" $ getGList book
		    pressEnter


addPerToGr matchingGuysBook= do persona <- whoFromResultsToEdit matchingGuysBook
				book <- getBook
				group <- promptString' "До якої групи додати контакт" (\x -> x `elem` (getGList book))
				saveNewBook $ addPersonToGroup book persona group
				pressEnter


removePerFromGr matchingGuysBook = do persona <- whoFromResultsToEdit matchingGuysBook
				      book <- getBook
	   			      group <- promptLine "З якої групи видалити"
				      saveNewBook $ removePersonFromGroup book persona group
				      pressEnter


removeGroup = do book <- getBook
		 group <- promptLine "Введіть назву групи"
		 if group `elem` getGList book
			then do saveNewBook $ deleteGroup book group
				putStrFlush "Групу видалено\n" >>pressEnter
			else putStrFlush "Такої групи не існує\n" >> pressEnter



groupChangeName = do book <- getBook
		     groupToChange <- promptString' "Введіть назву групи" (\g -> g `elem` getGList book)
		     newGroup <- promptString' "Нова назва" (\g -> not (g `elem` getGList book))
		     saveNewBook $ renameGroup book groupToChange newGroup
		     pressEnter



sumGroups =  do book <- getBook
		group1 <- promptString' "Введіть назву групи" (\g -> g `elem` getGList book)
		group2 <- promptString' "Назва групи для зʼєднання" (\g -> g `elem` getGList book)
		saveNewBook $ mergeGroups book group1 group2
		pressEnter


whoseBirthday = do book <- getBook
		   listP <- findBirthdayPeople book
		   showBook "Сьогодні день народження" (Phonebook listP [] [])
		   pressEnter

