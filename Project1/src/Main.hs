module Main (main) where

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

-- | Фунція для вставлення значення у список через кожні n елементiв
-- >> insertN 3 0 [3..12]
-- > [3,4,5,0,6,7,8,0,9,10,11,0,12]
--
-- >> insertN 2 'z' "abcdefg"
-- > "abzcdzefzg"
insertN :: Int -> a -> [a] -> [a]
insertN n el list
  | n == 0 || length list < n = list
  | null list = []
  | otherwise = take n list ++ [el] ++ insertN n el (drop n list)


-- | Функція для індексування списку елементів
-- >> indexed "abcdefg"
-- > [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7)]
--
-- >> indexed [1..9]
-- > [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9)]
indexed :: [a] -> [(a, Int)]
indexed ls = zip ls [1..length ls]


-- | Функція для перевірки якого елементу додавати
-- >> checkN ('a', 4) 2 '0'
-- > "0"
checkN :: (a, Int) -> Int -> a -> [a]
checkN x n el
   | snd x `mod` n == 0 = [el]
   | otherwise = []


-- | Функція для конкатенації елементу списку з необхідним елементом
-- >> generateEl ('a', 8) 2 'X'
-- > "aX"
--
-- >> (4, 8) 2 0
-- > [4,0]
generateEl :: (a, Int) -> Int -> a -> [a]
generateEl x n el = [fst x] ++ (checkN x) n el


-- |
-- >> mapping [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7)] 3 'X'
-- > "abcXdefXg
--
-- >> mapping [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9)] 3 0
-- > [1,2,3,0,4,5,6,0,7,8,9,0]
mapping :: [(a, Int)] -> Int -> a -> [a]
mapping ls n el = concat (map (\x -> generateEl x n el) ls)


-- | Фунція для вставлення значення у список через кожні n елементiв
-- >> m "abcdef" 3 'X'
-- > "abcXdefX"
--
-- >> m [1..9] 3 0
-- > [1,2,3,0,4,5,6,0,7,8,9,0]
m :: [a] -> Int -> a -> [a]
m ls n el = mapping (indexed ls) n el


-- 2.12 ----------------------------------------------------------------------------------------------------------------

isPrime2 :: Integer -> Bool
isPrime2 k = length [ x | x <- [2..k], k `mod` x == 0] == 1

getPrime :: [Integer] -> Maybe Integer
getPrime list = find isPrime list

getPrime2 :: [Integer] -> Maybe Integer
getPrime2 list = find isPrime2 list








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