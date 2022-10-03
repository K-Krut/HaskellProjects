module Main where

main :: IO ()
main = do
  putStrLn "hello world"


{-
Завдання 01
1. Дано список. Знайти:
  1.1 Кількість елементів
  1.2 Суму елементів
  1.3 Суму додатніх елементів
  1.4 Суму додатніх і від'ємних елементів
  1.5 Трансформувати у список пар. Напр. pairs [1,2,3,4,5] --> [(1,2),(3,4))
  1.6 Розділити на дві (приблизно) рівних частини, незалежно від порядку. Напр. half1 [1,2,3,4,5] --> ([1,3,5],[2,4])
  1.7 Розділити на дві (приблизно) рівних частини, збрегти вихідний порядок. Напр. half2 [1,2,3,4,5] --> ([1,2,3],[4,5])
  1.8 Додати задане значення після кожного елемента списку.
    Напр. ins 'a' "qwert" --> "qawaearata". ins 0 [1,2] --> [1,0,2,0]
-}





-- 1.1 -----------------------------------------------------------------------------------------------------------------
countList :: [Float] -> Int
countList list
  | null list = 0
  | otherwise = length list


-- 1.2 -----------------------------------------------------------------------------------------------------------------


getSum :: [Float] -> Float
getSum arr = sum arr

mySum :: [Float] -> Float
mySum [] = 0
mySum (x:xs) = x + mySum xs

fl xs = [ x | x <- xs, x > 0 ]
sumGen xs = sum $ fl xs



-- 1.3.1 ---------------------------------------------------------------------------------------------------------------

filterPositive :: [Float] -> [Float]
filterPositive arr = filter (\x -> x > 0) arr


getPositiveSum :: [Float] -> Float
getPositiveSum arr = sum (filterPositive arr)


getSum2 :: [Float] -> Float
getSum2 [] = 0
getSum2 (x:xs) | x > 0 = x + getSum2 xs
               | otherwise = getSum2 xs


-- 1.3.2 ---------------------------------------------------------------------------------------------------------------

filterNegative :: [Float] -> [Float]
filterNegative arr = filter (\x -> x < 0) arr


getNegativeSum :: [Float] -> Float
getNegativeSum arr = sum (filterNegative arr)


getSum3 :: [Float] -> Float
getSum3 [] = 0
getSum3 (x:xs) | x < 0 = x + getSum3 xs
               | otherwise = getSum3 xs


-- 1.4 -----------------------------------------------------------------------------------------------------------------

checkSign :: Char -> Float -> Bool
checkSign sign x
  | sign == '>' = x > 0
  | sign == '<' = x < 0
  | otherwise = error "Incorrect sign. Accaptable: '>' and '<'"



getSumSign :: [Float] -> Char -> Float
getSumSign [] sign = 0
getSumSign (x:xs) sign | checkSign sign x = x + getSumSign xs sign
                       | otherwise = getSumSign xs sign


-- 1.5 -----------------------------------------------------------------------------------------------------------------

pair :: [a] -> [(a, a)]
pair [] = []
pair [x] = []
pair (k:(v:t)) = (k, v) : pair t


-- 1.6 -----------------------------------------------------------------------------------------------------------------

split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:xs) = (x:y, z) where (z,y) = split xs


-- 1.7 -----------------------------------------------------------------------------------------------------------------

splitHalf :: [a] -> ([a], [a])
splitHalf list = splitAt (length list `div` 2) list



-- 1.8 -----------------------------------------------------------------------------------------------------------------

addItem str item = concat (map (\x -> [x] ++ [item]) str)

-- Example
fnc :: Int -> Char -> String -> String
fnc n c [] = []
fnc n c (x:xs) = c:x:(show n) ++ fnc n c xs


push el [] = []
push el (x:xs) = x:(show el) ++ push el xs