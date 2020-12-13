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
part2 = chinese . filter (\(_, n) -> n >= 0) . zip [1..] . tail

chinese :: [(Integer, Integer)] -> Integer
chinese numbers = shame 1 (go 1 numbers) n0
  where
    n0 = product (map fst numbers)
    go _ [] = 0
    go prev ((xm, x):xs) =
      let n1 = prev * product (map fst xs)
      in (x * shame 1 n1 xm) + go (xm * prev) xs
    shame i n m
      | (i * n) `mod` m == 1 = i * n
      | otherwise = shame (i + 1) n m
