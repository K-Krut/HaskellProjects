func1 :: [a] -> a
func1 (x:xs) = x

factors a = filter (isFactor a) [2..a-1]

isFactor a b = a `mod` b == 0

isPr 1 = False
isPr a = null $ factors a

gb2 :: Int -> (Int, Int)
gb2 a = head $ filter (\(x,y) -> isPr x && isPr y) $
                       map (\c -> (c, a - c)) [3,5..a `div` 2]



--checkTheory n ls (x:xs) = if any (\m -> m + x == n) ls then True else checkTheory n ls xs
checkTheory :: Int -> [Int] -> [Int] -> (Int, Int)
checkTheory n ls (x:xs)
    | any_ ls n x /= (0, 0) = any_ ls n x
    | otherwise = checkTheory n ls xs
    where
      any_ [] n k = (0, 0)
      any_ (x:xs) n k
          | x + k == n = (k, x)
          | otherwise = any_ xs n k

chTh :: Int -> [Int] -> (Int, Int)
chTh n ls
  | n < 2 = (0, 0)
  | odd n = (0, 0)
  | otherwise = checkTheory n ls ls


genPrimes :: Int -> [Int]
genPrimes n = [i | i <- [2..n], isPr i]


gb :: Int -> (Int, Int)
gb n = chTh n $ genPrimes n




--t = map (\c -> (c, a - c)) [3,5..a `div` 2]


{-

func1 :: [a] -> a
func1 (x:xs) = x

goldbach2 :: Int -> (Int, Int)
goldbach2 a = func1 $
                    filter (\(x,y) -> isPr x && isPr y) $
                    map (\c -> (c, a - c)) [3,5..a `div` 2]
  where
  factors a = filter (isFactor a) [2..a-1]
  isFactor a b = a `mod` b == 0
  isPr 1 = False
  isPr a = null $ factors a
-}

{-
Тут всё понятно:
Гипотеза: любое чётное (>2) представимо как сумма двух простых.
Берём список нечётных (простые всегда нечётные, кроме 2): [ 3, 5 ..]. Это будет первым слагаемым.
Если искомое число a, то второе слагаемое a-c (c - первое слагаемое).
Ясно, что список первого слагаемого можно ограничить a `div` 2, т.е. получаем [3,5..a `div` 2].
Дальше фунция map, получая каждый элемент списка (c) формирует пару слагаемых: с - первое, a-c - второе.
Это мы описали map (\c -> (c, a - c)) [3,5..a `div` 2]
Напр., для 10 получим список [(3,7), (5,5)].

Из этого списка надо выкинуть пары, где есть непростые (составные) числа.
Напр., для 12 список [(3,9), (5,7)] первая пара выкинется (фильтром), т.к. 9 - составное (делится на 3).
Если в результирующем списке несколько пар, то func1 просто берёт первую (напр., для 10 будет пара (3,7)). func1 это стандартная функция head.
Ну и вспомогательные функции: isPr - проверка на простое число, isFactor - на составное.

-}