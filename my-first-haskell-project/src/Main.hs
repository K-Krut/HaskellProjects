module Main where


main :: IO ()
main = do
  putStrLn "hello world"



-- Variant 12 ----------------------------------------------------------------------------------------------------------

{-
[[(Bool,String)],[Double]]
[[(True, "kek")], [3.6]]
-}


checkPoints :: Float -> Float -> Float -> Float -> Bool
checkPoints x1 y1 x2 y2 =
  x2 - x1 == y2 - y1

checkPointsTuple :: ((Float, Float), (Float, Float)) -> Bool
checkPointsTuple tuple = do
     let fstPoint = fst tuple
     let sndPoint = snd tuple
     fst sndPoint - fst fstPoint == snd




-- Variant 14 ----------------------------------------------------------------------------------------------------------


{-
[([Double],(Bool,Char),Integer)]
[([5.6], (True, "m"), 4)]
-}


checkRectangle :: Float -> Float -> Float -> Float -> Bool
checkRectangle side1 side2 side3 side4 =
  side1 == side2 && side3 == side4 || side1 == side3 && side2 == side4 || side1 == side4 && side3 == side2


checkRectangleArr :: [Float] -> Bool
checkRectangleArr arr
  | null arr = error "No values!"
  | length arr /= 4 = error "Must be transmitted 4 values"
  | otherwise = arr !! 0 == arr !! 1 && arr !! 2 == arr !! 3 || arr !! 0 == arr !! 2 && arr !! 1 == arr !! 3 || arr !! 0 == arr !! 3 && arr !! 2 == arr !! 1

---  | otherwise = do
--    let x = arr !! 0
--    let y = arr !! 1
--    let z = arr !! 2
--    let i = arr !! 3;
--    checkRectangle x y z i



















--checkPointsWithSquare :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float-> Bool
--checkPoints x1 y1 x2 y2 sx1 sy1 sx2 sy2  =
--  x2 - x1 == y2 - y1

--checkPointsTupleWithSquare :: ((Float, Float), (Float, Float), (Float, Float), (Float, Float)) -> Bool
--checkPointsTuple tuple = do
--     let fstPoint = fst tuple
--     let sndPoint = snd tuple
--     fst sndPoint - fst fstPoint == snd sndPoint - snd fstPoint