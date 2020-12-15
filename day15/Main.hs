module Main where

import Data.IntMap.Strict as IM (IntMap, empty, fromList, insert, lookup)
import Data.List (foldl')

input :: [Int]
input = [2, 15, 0, 9, 1, 20]

main :: IO ()
main = do
  putStrLn $ "Part 1: " <> show (run input 2020)
  putStrLn $ "Part 2: " <> show (run input 30000000)

run :: [Int] -> Int -> Int
run initNumbers finalTurn = numberAtTurn (length initNumbers + 1) initSeen (last initNumbers)
  where
    initSeen = fromList . flip zip [1..] $ init initNumbers
    numberAtTurn turn seen lastSpoken
      | turn > finalTurn = lastSpoken
      | otherwise = numberAtTurn (turn + 1) (insert lastSpoken (turn - 1) seen) $ maybe 0 (turn - 1 -) (IM.lookup lastSpoken seen)
