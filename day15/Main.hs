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
      | otherwise = case IM.lookup lastSpoken seen of
          Just turnSpoken -> let n = (turn - 1 - turnSpoken) in numberAtTurn (turn + 1) (insert lastSpoken (turn - 1) seen) n
          Nothing -> numberAtTurn (turn + 1) (insert lastSpoken (turn - 1) seen) 0
