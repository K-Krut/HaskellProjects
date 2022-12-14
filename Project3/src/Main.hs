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
		      ("Пошук контактів за даними", searchSubmenu Interface.pressEnter'),
		      ("Додати контакт", Interface.addContact),
		      ("Редагувати контакт", searchSubmenu Interface.editOrRemoveP),
          ("У кого сьогодні день народження?", Interface.whoseBirthday),
	        ("Переглянути зустрічі", Interface.printMeetingsFile),
	        ("Додати зустріч", Interface.addMeeting),
		      ("Редагувати зустріч", searchSubmenuMeeting Interface.editOrRemoveMeeting),
          ("Вихід", exitSuccess)]



searchSubmenu nextFunction = showMenu "Пошук контактів по:"
	              [("Імʼя",  Interface.find name nextFunction),
                ("Фамілія", Interface.find familyName nextFunction),
                ("Номер", Interface.find telephone nextFunction),
     	          ("Дата народження (d.m.rrrr)", Interface.find (printableDate.birthday) nextFunction),
		            ("<- Повернутись", main)]



searchSubmenuMeeting nextFunction = showMenu "Пошук зустрічей по:"
	              [("Імʼя",  Interface.findM namePlace nextFunction),
                ("Місце", Interface.findM place nextFunction),
     	          ("Дата проведення (d.m.rrrr)", Interface.findM (printableDate.dateMeeting) nextFunction),
		            ("Проведено?", Interface.findM held nextFunction),
		            ("<- Повернутись", main)]
