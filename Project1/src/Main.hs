module Main (main) where

import Data.Typeable
import Data.List
import Data.Numbers.Primes


main :: IO ()
main = do
  putStrLn "hello world"


{-
  Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
  застосуванням вбудованих функцiй.

  1.12 Вставити у список через кожнi n елементiв вказане значення, напр.
  через n=2 значення ’z’: "1234590"⇒ "12z34z59z0".

  2.12 Знайти перше просте число в указаному дiапазонi.
-}


-- 1.12 ----------------------------------------------------------------------------------------------------------------

insertN n el list
  | n == 0 || length list < n = list
  | list == [] = []
  | otherwise = take n list ++ [el] ++ insertN n el (drop n list)


ind x list = findIndex (==x) list

addItem str item = concat (map (\x -> [x] ++ [item]) str)

-- 2.12 ----------------------------------------------------------------------------------------------------------------

isPrime2 :: Integer -> Bool
isPrime2 k = length [ x | x <- [2..k], k `mod` x == 0] == 1

getPrime :: [Integer] -> Maybe Integer
getPrime list = find isPrime list

getPrime2 :: [Integer] -> Maybe Integer
getPrime2 list = find isPrime2 list

checkN x n el
   | snd x `mod` n == 0 = [el]
   | otherwise = []


indexed ls = zip ls [1..length ls]
generateEl x n el = [fst x] ++ (checkN x) n el
mp ls n el = concat (map (\x -> generateEl x n el) ls)
m ls n el = mp (indexed ls) n el
-- m "abcdef" 3 'X'
-- m [1..9] 3 0



{-
  indexed ls = zip ls [1..length ls]
  generateEl x = [fst x] ++ (checkN x) 3 'X'
  mp ls = concat (map (\x -> generateEl x) ls)
  m ls = mp (indexed ls)


  indexed ls = zip ls [1..length ls]
  generateEl x n el = [fst x] ++ (checkN x) n el
  mp ls n el = concat (map (\x -> generateEl x n el) ls)
  m ls = mp (indexed ls) 3 'X'


  -- | "abcdf"
  -- | mp [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6)]
  -- | "abcXdefX"
  -- | "abcXdefX"
-}