{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (groupBy, minimumBy)
import Data.Ord (comparing)
import Control.Applicative ((<|>))

main :: IO ()
main = do
    (earliest, busIDs) <- parse . lines <$> readFile "input.txt"
    putStrLn $ "Part 1: " <> show (part1 earliest busIDs)
    putStrLn $ "Part 2: " <> show (part2 busIDs)
  where
    parse :: [String] -> (Int, [Int])
    parse [e, b] = (read e, map read' . filter (\a -> a /= ",") $ groupBy (\a b -> a /= ',' && b /= ',') b)
    read' x = if x == "x" then -1 else read x

part1 :: Int -> [Int] -> Int
part1 earliest = uncurry (*) . minimumBy (comparing snd) . map (id &&& minutesToWait) . filter (>= 0)
  where
    minutesToWait x = x - earliest `mod` x

part2 :: [Int] -> Int
part2 (x:xs) = solve
  where
    solve = until' (\k -> findEarliest (x * k) constrained)
    constrained = filter (\b -> fst b /= -1) $ zip xs [1..]
    findEarliest !xk [] = Just xk
    findEarliest !xk ((!busID, !i):bs)
      | (xk + i) `mod` busID == 0 = findEarliest xk bs
      | otherwise = Nothing

-- At first I was using <|>, but it accumulates thinks in memory :()
until' :: (Int -> Maybe b) -> b
until' f = go 1
  where
    go k = case f k of
      Just r -> r
      Nothing -> go (k + 1)
