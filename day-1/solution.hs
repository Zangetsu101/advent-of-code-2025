module Main where

operate :: Integer -> String -> Integer
operate current ('L' : numStr) =
  let num = read numStr :: Integer
   in (current - num + 100) `mod` 100
operate current ('R' : numStr) =
  let num = read numStr :: Integer
   in (current + num) `mod` 100

partOne = length . filter (== 0) . scanl operate 50 . lines

partTwo = sum . map snd . scanl operateWithCrossings (50, 0) . lines
 where
  operateWithCrossings (current, _) operation@('L' : numStr) =
    let num = read numStr :: Integer
        newCurrent = operate current operation
        crossings = num `div` 100 + if current > 0 && current - (num `mod` 100) < 0 then 1 else 0
     in (newCurrent, crossings + if newCurrent == 0 then 1 else 0)
  operateWithCrossings (current, crossings) operation@('R' : numStr) =
    let num = read numStr :: Integer
        newCurrent = operate current operation
        crossings = num `div` 100 + if current + (num `mod` 100) > 100 then 1 else 0
     in (newCurrent, crossings + if newCurrent == 0 then 1 else 0)

main :: IO ()
main = interact $ show . partTwo
