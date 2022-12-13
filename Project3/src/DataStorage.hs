module DataStorage (loadBook,overwriteBook) where

import Data.List
import System.IO
import System.Directory
import Terminal
import Phonebook

tempFile = "temp"


createFname fname = ("db/" ++ fname ++ ".data")



--
--loadBook  =   do System.Directory.createDirectoryIfMissing True "db/"
--                        handleFile <- openFile ("db/contacts.data") ReadWriteMode
--                        idata <- loadData handleFile
--                        return idata




loadBook :: [Char] -> IO Phonebook
loadBook fname =   do   System.Directory.createDirectoryIfMissing True "db/"
                        handleFile <- openFile ("db/contacts.data") ReadWriteMode
--                        handleFile <- openFile (createFname fname) ReadWriteMode
                        idata <- loadData handleFile
                        return idata



loadData handleFile = do isEof <- hIsEOF handleFile
	                 if isEof then
	                     return (Phonebook [] [] [])
	                 else do
	                     contents <- hGetContents handleFile
	                     return (read (contents)::Phonebook)


overwriteBook newbook dataFile = do
	writeFile tempFile' (show newbook)
	x <- doesFileExist dataFile'
	if x
	  then removeFile dataFile' >> renameFile tempFile' dataFile'
	  else renameFile tempFile' dataFile'
		where tempFile' = (createFname tempFile)
		      dataFile' = (createFname dataFile)
		      

--
--kek = do
--        handleFile <- openFile ("db/contacts.data") ReadWriteMode
--        contents <- hGetContents handleFile
--        kkk <- read (contents)::Phonebook
--        return kkk
----        return (read (contents)::Phonebook)