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

checkType el
   | show (typeOf el) == "Char" = 1
   | show (typeOf el) == "[Char]" = 0
   | otherwise = -1


insertN n el list
  | n == 0 || length list < n = list
  | list == [] = []
  | otherwise = take n list ++ [el] ++ insertN n el (drop n list)


addItem str item = concat (map (\x -> [x] ++ [item]) str)


--insertAtN n y xs = intercalate [y] . groups n $ xs
--  where
--    groups n xs = takeWhile (not.null) . unfoldr (Just . splitAt n) $ xs


-- 2.12 ----------------------------------------------------------------------------------------------------------------

isPrime2 :: Integer -> Bool
isPrime2 k = length [ x | x <- [2..k], k `mod` x == 0] == 1

getPrime :: [Integer] -> Maybe Integer
getPrime list = find isPrime list

getPrime2 :: [Integer] -> Maybe Integer
getPrime2 list = find isPrime2 list

