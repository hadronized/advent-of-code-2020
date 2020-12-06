module Main where

import Data.List (tails)

main :: IO ()
main = do
  numbers <- fmap (map read . lines) (readFile "input.txt")
  print . head $ [a * b | x <- tails numbers, y <- tails x, (a, b) <- zip x y, a + b == 2020]
  print . head $ [a * b * c | x <- tails numbers, y <- tails x, z <- tails y, (a, b, c) <- zip3 x y z, a + b + c == 2020]
