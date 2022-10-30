module Main (main) where


import Data.List (unfoldr)
import Data.Maybe (listToMaybe)



main :: IO ()
main = do
  putStrLn "hello world"


{-
Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
застосуванням вбудованих функцiй вищого порядку.


1.12 Роздiлити список на двi частини при заданiй довжинi першої n, напр.
при n=3: "abcdefghik"⇒ ("abc "defghik")

2.12 Перевiрити гiпотезу Ґольдбаха у вказаному дiапазонi.

-}


-- 1.12 ----------------------------------------------------------------------------------------------------------------

--Functions that take other functions as arguments, or return other functions, are called higher-order functions
--



splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _  []     = ([], [])
splitAt' 1  (x:xs) = ([x], xs)
splitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs


split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:xs) = (x:y, z) where (z,y) = split xs


splitN :: Int -> [a] -> ([a], [a])
splitN n list = splitAt n list


spN n xs = (take n xs, drop n xs)


-- 2.12 ----------------------------------------------------------------------------------------------------------------








--функция фычисляет список всех делителей числа
factors :: Integer -> [Integer]
factors n = [k | k <- [1..n], n `mod` k == 0]


--функция определяет является ли число простым
isprime :: Integer -> Bool
isprime n = factors n == [1, n]


--функция возвращает  список, содержащий все простые числа до числа n включительно
prs :: Integer -> [Integer]
prs n = [k | k <- [3..n], factors n == [1, n]]
--prs n = [k | k <- [3..n], isprime k]


--ch n _ = []
--ch n (x:xs) =
-- [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]
-- [2, 3, 5, 7]



k :: Int -> [Int] -> [Int] -> Bool
--k n ls _ = False
--k _ ls xs = False
k _ _ _ = False
k n ls xs = if any (\x -> x + ls !! 0 == n ) xs then True else k n (drop 1 ls) xs
--k ls xs = if any (\x -> x + ls !! 0 == 12) xs then True else k (drop 1 ls)
--k ls = if any (\m -> m + take 1 ls == 12) ls then True else k (drop 1 ls)






{-
k _ _ _ = False
k n ls xs =
  | n < 3 = False
  | odd n = False
  | otherwise if any (\x -> x + ls !! 0 == n ) xs then True else k (drop 1 ls) xs
-}

