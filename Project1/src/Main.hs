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




--getNumber = do putStrLn "Enter the number:"
--               number <- readLn
--               case findposition number box of
--                 Just n  -> -- Do whatever
--                 Nothing -> putStrLn "Please try again." >> getNumber


--checkN x list n el
--   | elemIndex x list `mod` n == 0 = [el]
--   | otherwise = []


--insertN2 0 el list = list
--insertN2 n el list = concat (map (\x -> [x] ++ checkN x list n el) list)


--insertN2 n el list
-- | n <= 0 || length list < n = list
-- | otherwise = concat (map (\x -> [x] ++ [elemIndex x list]) list)

-- | otherwise = concat (map (\x -> [x] ++ if elemIndex x list `mod` n == 0 then [el] else []) list)
-- | otherwise = concat (map (\x -> [x] ++ checkN x list n el) list)

--checkN x list n el
--   | case ind x of list `mod` n == 0 = [el]
--   | otherwise = []

ind x list = findIndex (==x) list

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


--
--checkN x
--   | x /= 0 && x `mod` 3 == 0 = "X"
--   | otherwise = []

--checkN :: Integer -> Integer -> a -> [a]
checkN x n el
   | snd x `mod` n == 0 = [el]
   | otherwise = []

--mp str = concat (map (\x -> [x] ++ "X") str)
--mp str = concat (map (\x -> [fst x] ++ show [snd x]) str)
--mp str = concat (map (\x -> [fst x] ++ checkN snd x 3 "X") str)

--indexed :: [a] -> [(a, Integer)]
indexed ls = zip ls [1..length ls]

generateEl x = [fst x] ++ (checkN x) 3 'X'

mp ls = concat (map (\x -> generateEl x) ls)

m ls = mp (indexed ls)


-- | "abcdf"
-- | mp [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6)]
-- | "abcXdefX"
-- | "abcXdefX"