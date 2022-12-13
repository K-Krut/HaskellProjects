import System.IO
import System.Exit
import Control.Monad

import Phonebook
import Terminal
import Interface
import Person
import Meeting
import Date

-- **********  main menu  ***********
main =  forever $ showMenu "MENU GŁÓWNE" 
	       [("Переглянути контакти", Interface.printContactsFile),
		("Переглянути групи",  Interface.printGroup),
		("Пошук контактів за даними", searchSubmenu Interface.pressEnter'),
		("Додати контакт", Interface.addContact),
		("Редагування контатів", searchSubmenu Interface.editOrRemoveP),
                ("Редагувати групу", groupsSubmenu),
                ("Kto ma dzisiaj urodziny?", Interface.whoseBirthday),
                ("Вихід", exitSuccess)]

-- ******* searchSubmenu *******
searchSubmenu nextFunction = showMenu "Пошук контактів по:"
	       [("Імʼя",  Interface.find name nextFunction),
                ("Фамілія", Interface.find familyName nextFunction),
                ("Компанія", Interface.find company nextFunction),
                ("Номер", Interface.find telephone nextFunction),
 		("Email", Interface.find mail nextFunction),
     	        ("Дата народження (d.m.rrrr)", Interface.find (printableDate.birthday) nextFunction),
		("<- Повернутись", main)]

-- ********* groupsSubmenu **********
groupsSubmenu= showMenu "EDYCJA GRUP" 
	       [("Додати групу", Interface.newGroup),
		("Переглянути існуючі групи", Interface.printAllGroups),
		("Додати контакт до групи", searchSubmenu Interface.addPerToGr),
		("Видалити контакт з групи", searchSubmenu Interface.removePerFromGr),
                ("Змінити назву групи", Interface.groupChangeName),
		("Обʼєднати 2 групи", Interface.sumGroups),
                ("Видалити групу", Interface.removeGroup),
                ("<- Повернутись", main)]
