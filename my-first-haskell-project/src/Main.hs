module Main where


import Control.Exception

main :: IO ()
main = do
  putStrLn "hello world"

addOne :: Integer -> Integer
addOne x = x + 1



checkRectangle :: Float -> Float -> Float -> Float -> Bool
checkRectangle side1 side2 side3 side4 =
  side1 == side2 && side3 == side4 || side1 == side3 && side2 == side4 || side1 == side4 && side3 == side2



--checkTupleRectangle :: (Float -> Float -> Float -> Float) -> Bool
--checkTupleRectangle (side1, side2, side3, side4) =
--  side1 == side2 && side3 == side4 || side1 == side3 && side2 == side4 || side1 == side4 && side3 == side2

checkRectangleArr :: [Float] -> IO()
checkRectangleArr arr = do
  if null arr
    then  error "Oops!" else putStrLn $ show arr
