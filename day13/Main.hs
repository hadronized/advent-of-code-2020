{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Foldable (find)
import Data.List (groupBy, minimumBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)

main :: IO ()
main = do
    (earliest, busIDs) <- parse . lines <$> readFile "input.txt"
    putStrLn $ "Part 1: " <> show (part1 earliest busIDs)
    putStrLn $ "Part 2: " <> show (part2 busIDs)
  where
    parse :: [String] -> (Integer, [Integer])
    parse [e, b] = (read e, map read' . filter (\a -> a /= ",") $ groupBy (\a b -> a /= ',' && b /= ',') b)
    read' x = if x == "x" then -1 else read x

part1 :: Integer -> [Integer] -> Integer
part1 earliest = uncurry (*) . minimumBy (comparing snd) . map (id &&& minutesToWait) . filter (>= 0)
  where
    minutesToWait x = x - earliest `mod` x

part2 :: [Integer] -> Integer
part2 (x:xs) = solve 0 x (constrained xs)
  where
    constrained = filter ((>= 0) . snd) . zip [1..]
    solve c p [] = c
    solve c p ((w, b):ks) = let c' = findT c p w b in solve c' (p * b) ks

findT :: Integer -> Integer -> Integer -> Integer -> Integer
findT c p w b = go 1
  where
    go i
      | (p * i + c + w) `mod` b == 0 = p * i + c
      | otherwise = go (i + 1)
