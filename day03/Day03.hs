module Main where

import Data.List (foldl')
import Numeric.Natural (Natural)

main :: IO ()
main = do
  lines <- fmap lines (readFile "input.txt")
  let c = trees lines 3 1
  print c
  print $ c * (product $ map (uncurry $ trees lines) [(1, 1), (5, 1), (7, 1), (1, 2)])

trees :: [String] -> Int -> Int -> Natural
trees l x y = let (_, count) = foldl' f (0, 0) (every y l) in count
  where
    f (px, count) line = ((px + x) `mod` w, count + if line !! px == '#' then 1 else 0)
    w = length (head l)

every :: Int -> [a] -> [a]
every n = go 0
  where
    p = n - 1
    go _ [] = []
    go 0 (x:xs) = x : go p xs
    go i (_:xs) = go (i - 1) xs
