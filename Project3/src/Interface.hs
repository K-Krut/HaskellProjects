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
		else putStrFlush "Taka osoba już jest w książce, kontakt nie został dodany"


getPersonData = do
		name <- promptString' "Imię" validName
	        familyName <- promptString' "Nazwisko" validName
		company  <- promptLine "Firma"
		telephone  <- promptString' "Nr telefonu" validPhone
		mail  <- promptString' "Adres email" validMail
		birthday  <- promptString' "Data urodzin(dd.mm.rrrr)" validDate
		return $ Person name familyName company telephone mail (stringToDate birthday) []


addMeeting :: IO ()
addMeeting = do book <- getBook
		meetingToAdd <- getMeetingData
		if not (meetingToAdd `elem` (getMList book))
		  then saveNewBook $ addMeetingData book meetingToAdd
		else putStrFlush "Taka osoba już jest w książce, kontakt nie został dodany"


getMeetingData = do
		namePlace <- promptString' "Name" validName
	        place <- promptString' "Place" validName
		dateMeeting <- promptString' "Data urodzin(dd.mm.rrrr)" validDate
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
		groupName <- promptLine "Podaj nazwę grupy"
		showBook "Kontakty w tej grupie" $ (Phonebook (findPeopleInGroup book groupName) [] [])
		pressEnter


printContactsFile = getBook >>= showBook "Wszystkie kontakty"  >> pressEnter

printMeetingsFile = getBook >>= showBookM "Meetings"  >> pressEnter


find byWhat functionAtEnd= do book <- getBook
			      value <- promptLine "Podaj prefix"
			      showBook "WYNIKI" (Phonebook (findPeopleBy byWhat value book) [] [])
			      functionAtEnd (Phonebook (findPeopleBy byWhat value book) [] [])


findM byWhat functionAtEnd = do
      book <- getBook
      value <- promptLine "Префікс:"
      showBook "Результати" (Phonebook [] [] (findMeetingBy byWhat value book))
      functionAtEnd (Phonebook [] [] (findMeetingBy byWhat value book))



pressEnter':: Phonebook -> IO ()
pressEnter' whatever = promptLine "ENTER.." >> return ()


whoFromResultsToEdit matchingGuysBook = if (inputLength > 1)
						then do nr <- prompt' "Podaj numer kontaktu do edycji"  (\c -> c >= 1 && c <= inputLength)
							return $ getPerson matchingGuysBook nr
						else return $ getPerson matchingGuysBook 1
							where inputLength = length (getPList matchingGuysBook)


whoFromResultsToEditM matchingMeetings = if (inputLength > 1)
						then do nr <- prompt' "Номер зістрічі"  (\c -> c >= 1 && c <= inputLength)
							return $ getMeeting matchingMeetings nr
						else return $ getMeeting matchingMeetings 1
							where inputLength = length (getMList matchingMeetings)


editOrRemoveP matchingGuysBook = do persona <- whoFromResultsToEdit matchingGuysBook
				    editOrDelete <- prompt' "Опції:\n 1) Редагувати\n 2) Видалити\n 3) Відмінити\n "  (\c -> c `elem` [1,2,3])
				    resultAction editOrDelete persona
					where resultAction x persona = case x of
								    1 -> editContact persona
								    2 -> deleteContact persona
								    3 -> return ()


editOrRemoveMeeting matchingMeetings = do
      meeting <- whoFromResultsToEditM matchingMeetings
      editOrDelete <- prompt' "Опції:\n 1) Редагувати\n 2) Видалити\n 3) Відмінити\n "  (\c -> c `elem` [1,2])
      resultAction editOrDelete meeting
        where resultAction x meeting = case x of
                1 -> Interface.editMeeting meeting
                2 -> deleteMeeting meeting
                3 -> return ()



editContact :: Person -> IO ()
editContact oldPerson = do book <- getBook
			   putStrFlush "Podaj nowe dane kontaktu:\n"
			   newPerson <- getPersonData
			   saveNewBook $ editPerson book oldPerson newPerson
			   putStrFlush "\t\t\t ---------------> Kontakt został zmieniony!\n" >> pressEnter


editMeeting :: Meeting -> IO ()
editMeeting oldM = do
      book <- getBook
      putStrFlush "Podaj nowe dane kontaktu:\n"
      newM <- getMeetingData
      saveNewBook $ Phonebook.editMeeting book oldM newM
      putStrFlush "\t\t\t ---------------> Kontakt został zmieniony!\n" >> pressEnter


deleteContact :: Person -> IO ()
deleteContact personToDel = do book <- getBook
			       saveNewBook $ removePerson book personToDel
			       putStrFlush "\t\t\t ---------------> Kontakt został usunięty!\n" >> pressEnter


deleteMeeting :: Meeting -> IO ()
deleteMeeting meetingToDel = do
              book <- getBook
              saveNewBook $ removeMeeting book meetingToDel
              putStrFlush "\t\t\t ---------------> Kontakt został usunięty!\n" >> pressEnter


newGroup = do book <- getBook
	      group <- promptLine "Введіть назву групи"
	      saveNewBook $ addGroup book group


printAllGroups = do book <- getBook
		    showItems "Istniejące grupy" $ getGList book
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
		 group <- promptLine "Podaj nazwę grupy do usunięcia"
		 if group `elem` getGList book
			then do saveNewBook $ deleteGroup book group
				putStrFlush "Grupa usunieta!\n" >>pressEnter
			else putStrFlush "Taka grupa nie isnieje\n" >> pressEnter



groupChangeName = do book <- getBook
		     groupToChange <- promptString' "Podaj nazwę istniejącej grupy do zmiany" (\g -> g `elem` getGList book)
		     newGroup <- promptString' "Podaj nową nazwę (inną niż istniejące)" (\g -> not (g `elem` getGList book))
		     saveNewBook $ renameGroup book groupToChange newGroup
		     pressEnter



sumGroups =  do book <- getBook
		group1 <- promptString' "Podaj nazwę istniejącej grupy która ma zostać włączona do innej" (\g -> g `elem` getGList book)
		group2 <- promptString' "Podaj nazwę drugiej istniejącej grupy do scalenia" (\g -> g `elem` getGList book)
		saveNewBook $ mergeGroups book group1 group2
		pressEnter


whoseBirthday = do book <- getBook
		   listP <- findBirthdayPeople book
		   showBook "Dzisiaj urodzinki mają:" (Phonebook listP [] [])
		   pressEnter

