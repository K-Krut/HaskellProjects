module Terminal where
import System.IO
import Control.Exception
import Phonebook
import Person
import Meeting


putStrFlush str =  do  putStr str
                       hFlush stdout


promptLine what =   do  putStrFlush $ what ++ ": "
                        x <- System.IO.getLine
                        return x


prompt' :: Read b => [Char] -> (b -> Bool) -> IO b
prompt' text f =    do  putStrFlush $ text ++ ": "
                        x <- try readLn :: (Read t0) => IO (Either SomeException t0)
                        case x of
                            Left e  ->  invalid
                            Right v ->  if (f v) then return v
                                        else invalid
  where invalid = putStrFlush "Помилка, спробуй ще раз: " >> prompt' text f
	
	

promptString' :: [Char] -> ([Char] -> Bool) -> IO [Char]
promptString' text f =    do  putStrFlush $ text ++ ": "
                              x <- getLine
                              if ( f x ) then return x
					 else  putStrFlush "Помилка, спробуй ще раз: " >> promptString' text f



createLabel label size fill =   let fillsize = (size - (length label) - 2)
                                    halffillL = fillsize `div` 2
                                    halffillR = halffillL + (fillsize `mod` 2)
                                in
                                    (replicate halffillL fill) ++ " " ++ label ++ " " ++ (replicate halffillR fill)   


showItems title itemsList = showItemsAndComment title itemsList []



showItemsAndComment title itemsList  comment =
    do  putStrLn $ "\n" ++ createLabel title 100 '-'
        if (comment /= []) then do
            putStrFlush $ " *) " ++ comment ++ "\n"
            putStrFlush $ menuText itemsList 1
        else
            putStrFlush $ menuText itemsList 1
        putStrLn $ createLabel "-" 100 '-'
        where
            menuText [] inum = []
            menuText (i:is) inum = " " ++ (show inum) ++ ") " ++ i ++ "\n" ++ menuText is (inum + 1)                                  


showBook title (Phonebook pList gList mList) =
    do  putStrLn $ "\n" ++ createLabel title 100 '-'
        putStrFlush $ " *) " ++ comment ++ "\n"
        putStrFlush $ bookText pList 1
        putStrFlush $ bookTextM mList 1
        putStrLn $ createLabel "-" 100 '-'
        where
            bookText [] _ = []
            bookText (i:is) inum = " " ++ (show inum) ++ ") " ++ (printablePerson i) ++ "\n" ++ bookText is (inum + 1)

            bookTextM [] _ = []
            bookTextM (i:is) inum = " " ++ (show inum) ++ ") " ++ (printableMeeting i) ++ "\n" ++ bookTextM is (inum + 1)
	    comment = "Імʼя | Фамілія | #Телефону | День народження | Група"



showBookM title (Phonebook pList gList mList) =
    do  putStrLn $ "\n" ++ createLabel title 100 '-'
        putStrFlush $ " *) " ++ comment ++ "\n"
        putStrFlush $ bookTextM mList 1
        putStrLn $ createLabel "-" 100 '-'
        where
            bookTextM [] _ = []
            bookTextM (i:is) inum = " " ++ (show inum) ++ ") " ++ (printableMeeting i) ++ "\n" ++ bookTextM is (inum + 1)
	    comment = "Назва  |  Місце  |  Дата  |  Проведено?"




pressEnter = promptLine "ENTER <--" >> return ()



showMenu :: [Char] -> [([Char], IO a)] -> IO a
showMenu title actionList = do  showItems title [fst i | i <- actionList]
                                choice <- prompt' "Твій вибір" (\c -> c >= 1 && c <= length actionList)
                                snd $ actionList !! (choice - 1)  




 
