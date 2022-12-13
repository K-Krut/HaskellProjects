import System.IO
import System.Exit
import Control.Monad

import Phonebook
import Terminal
import Interface
import Person
import Meeting
import Date



main =  forever $ showMenu "Меню"
	       [("Переглянути контакти", Interface.printContactsFile),
		      ("Переглянути групи",  Interface.printGroup),
		      ("Пошук контактів за даними", searchSubmenu Interface.pressEnter'),
		      ("Додати контакт", Interface.addContact),
		      ("Редагувати контакт", searchSubmenu Interface.editOrRemoveP),
          ("Редагувати групу", groupsSubmenu),
          ("У кого сьогодні день народження?", Interface.whoseBirthday),
	        ("Переглянути зустрічі", Interface.printMeetingsFile),
	        ("Додати зустріч", Interface.addMeeting),
		      ("Редагувати зустріч", searchSubmenuMeeting Interface.editOrRemoveMeeting),
          ("Вихід", exitSuccess)]



searchSubmenu nextFunction = showMenu "Пошук контактів по:"
	              [("Імʼя",  Interface.find name nextFunction),
                ("Фамілія", Interface.find familyName nextFunction),
                ("Компанія", Interface.find company nextFunction),
                ("Номер", Interface.find telephone nextFunction),
 		            ("Email", Interface.find mail nextFunction),
     	          ("Дата народження (d.m.rrrr)", Interface.find (printableDate.birthday) nextFunction),
		            ("<- Повернутись", main)]



searchSubmenuMeeting nextFunction = showMenu "Пошук зустрічей по:"
	              [("Імʼя",  Interface.findM namePlace nextFunction),
                ("Місце", Interface.findM place nextFunction),
     	          ("Дата проведення (d.m.rrrr)", Interface.findM (printableDate.dateMeeting) nextFunction),
		            ("Проведено?", Interface.findM held nextFunction),
		            ("<- Повернутись", main)]


groupsSubmenu= showMenu "Меню Груп"
	       [("Додати групу", Interface.newGroup),
		      ("Переглянути існуючі групи", Interface.printAllGroups),
		      ("Додати контакт до групи", searchSubmenu Interface.addPerToGr),
		      ("Видалити контакт з групи", searchSubmenu Interface.removePerFromGr),
          ("Змінити назву групи", Interface.groupChangeName),
		      ("Обʼєднати 2 групи", Interface.sumGroups),
          ("Видалити групу", Interface.removeGroup),
          ("<- Повернутись", main)]
