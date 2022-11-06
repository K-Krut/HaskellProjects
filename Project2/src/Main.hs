module Main (main) where


import Data.List (unfoldr)
import Data.Maybe (listToMaybe)
--import Data.Numbers.Primes


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



checkTheory :: Int -> [Int] -> [Int] -> Bool
checkTheory n ls xs = if any (\x -> x + xs !! 0 == n) ls then True else checkTheory n ls (drop 1 xs)

chTh :: Int -> [Int] -> Bool
chTh n ls
  | n < 2 = False
  | odd n = False
  | otherwise = checkTheory n ls ls


is_prime :: Int -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
		       | otherwise = True

genPrimes :: Int -> [Int]
genPrimes n = [i | i <- [2..n], is_prime i]


goldbach :: Int -> Bool
goldbach n = chTh n $ genPrimes n


