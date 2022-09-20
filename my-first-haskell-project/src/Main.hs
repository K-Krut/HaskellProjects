module Main where


main :: IO ()
main = do
  putStrLn "hello world"


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
---