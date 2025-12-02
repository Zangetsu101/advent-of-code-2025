module Main where

operate :: Integer -> String -> Integer
operate current ('L': numStr) =
  let num = read numStr :: Integer
  in (current - num + 100) `mod` 100
operate current ('R': numStr) =
  let num = read numStr :: Integer
  in (current + num) `mod` 100

partOne = length . filter (== 0) . scanl operate 50 . lines

main :: IO ()
main = interact $ show . partOne
