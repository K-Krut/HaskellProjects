module Main (main) where

-- Import the necessary modules
import System.IO
import Control.Monad
import System.Directory
import Data.List.Split
import Data.List
--import Data.List.Split


createName name = "data/" ++ name ++ ".txt"


readFromFile = do
      return readFile $ createName "input"


writeToFile inputData = do
       writeFile (createName "output") inputData


promptLine  = do
      x <- System.IO.getLine
      return x


splitN n list = Data.List.splitAt n list


splitLine inputData = splitOn " " inputData

convertToInt index = read index :: Int

splitData inputData = do
      let ls = splitLine inputData
      let ind_ = ls !! 1
      let str_ = ls !! 0
      let index  = convertToInt ind_
      let result = splitN index str_
      return result


printToScreen inputData = do
  putStrLn $ show splitData inputData


main :: IO ()
main = do
  putStrLn "Choose input method: [1] Keyboard, [2] File"
  inputMethod <- getLine


  inputData <- case inputMethod of
    "1" -> getLine
    "2" -> readFile "data/input.txt"
    _   -> error "Invalid input method"



  putStrLn "Choose output method: [1] Screen, [2] File"
  outputMethod <- getLine


  case outputMethod of
    "1" -> splitData inputData
    "2" -> writeFile "data/output.txt" inputData
    _   -> error "Invalid input method"



  putStrLn "Enter [1] to continue or [2] to exit"
  continue <- getLine
  when (continue /= "2") main