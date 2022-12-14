module Interface (addContact,
      addMeeting,
		  printContactsFile,
		  printMeetingsFile,
		  find,
		  findM,
		  pressEnter', 
		  editOrRemoveP,
		  editOrRemoveMeeting,
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

getPList (Phonebook pList mList) = pList

getMList (Phonebook pList mList) = mList


addContact :: IO ()module Interface (addContact,
                         addMeeting,
                   		  printContactsFile,
                   		  printMeetingsFile,
                   		  find,
                   		  findM,
                   		  pressEnter',
                   		  editOrRemoveP,
                   		  editOrRemoveMeeting,
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

                   getPList (Phonebook pList mList) = pList

                   getMList (Phonebook pList mList) = mList


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
                   		return $ Person name familyName telephone (stringToDate birthday)


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


                   printContactsFile = getBook >>= showBook "Контакти"  >> pressEnter

                   printMeetingsFile = getBook >>= showBookM "Зустрічі"  >> pressEnter


                   find byWhat functionAtEnd= do book <- getBook
                   			      value <- promptLine "Префікс:"
                   			      showBook "Результати" (Phonebook (findPeopleBy byWhat value book) [])
                   			      functionAtEnd (Phonebook (findPeopleBy byWhat value book) [])


                   findM byWhat functionAtEnd = do
                         book <- getBook
                         value <- promptLine "Префікс:"
                         showBook "Результати" (Phonebook [] (findMeetingBy byWhat value book))
                         functionAtEnd (Phonebook [] (findMeetingBy byWhat value book))



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


                   whoseBirthday = do book <- getBook
                   		   listP <- findBirthdayPeople book
                   		   showBook "Сьогодні день народження" (Phonebook listP [])
                   		   pressEnter


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
		return $ Person name familyName telephone (stringToDate birthday)


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


printContactsFile = getBook >>= showBook "Контакти"  >> pressEnter

printMeetingsFile = getBook >>= showBookM "Зустрічі"  >> pressEnter


find byWhat functionAtEnd= do book <- getBook
			      value <- promptLine "Префікс:"
			      showBook "Результати" (Phonebook (findPeopleBy byWhat value book) [])
			      functionAtEnd (Phonebook (findPeopleBy byWhat value book) [])


findM byWhat functionAtEnd = do
      book <- getBook
      value <- promptLine "Префікс:"
      showBook "Результати" (Phonebook [] (findMeetingBy byWhat value book))
      functionAtEnd (Phonebook [] (findMeetingBy byWhat value book))



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


whoseBirthday = do book <- getBook
		   listP <- findBirthdayPeople book
		   showBook "Сьогодні день народження" (Phonebook listP [])
		   pressEnter

