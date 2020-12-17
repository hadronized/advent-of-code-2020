module Main where

import Control.Monad (replicateM)
import Data.Foldable (foldl')
import Data.Map as M (Map, empty, foldlWithKey, insert, insertWith, lookup, mapWithKey)

type Universe = Map [Int] Bool

main :: IO ()
main = do
    universe <- parseUniverse <$> readFile "input.txt" -- I have no shame
    universe' <- parseUniverse4 <$> readFile "input.txt" -- lol
    let universe6 = iterate step universe !! 6
    let universe6' = iterate step universe' !! 6
    putStrLn $ "Part 1: " <> show (countAlive universe6)
    putStrLn $ "Part 2: " <> show (countAlive universe6')
  where
    parseUniverse = foldl' parseSlice3 empty . zip [0..] . lines
    parseSlice3 universe (y, line) = foldl' parseCell universe $ zip [0..] line
      where
        parseCell universe (x, '#') = insert [x, y, 0] True universe
        parseCell universe (x, '.') = insert [x, y, 0] False universe
    parseUniverse4 = foldl' parseSlice4 empty . zip [0..] . lines
    parseSlice4 universe (y, line) = foldl' parseCell universe $ zip [0..] line
      where
        parseCell universe (x, '#') = insert [x, y, 0, 0] True universe
        parseCell universe (x, '.') = insert [x, y, 0, 0] False universe

step :: Universe -> Universe
step = mutate . inflate
  where
    -- inflate a universe; what it means is that for a given universe, it will automatically generate void cells on its
    -- border
    inflate = flip foldlWithKey empty $ \newUniverse p cellActive ->
      if cellActive
      then
        insert p True $ foldl' (\universe p' -> insertWith (const id) p' False universe) newUniverse (neighbors p)
      else
        newUniverse
    -- mutate a universe
    mutate universe = flip mapWithKey universe $ \p cellActive ->
      let aliveCount = length . filter (\(_, a) -> a) $ neighborCells universe p
      in if cellActive then aliveCount == 2 || aliveCount == 3 else aliveCount == 3

countAlive :: (Functor f, Foldable f, Enum a) => f a -> Int
countAlive = sum . fmap fromEnum

neighbors :: [Int] -> [[Int]]
neighbors p = tail [zipWith (+) p x | x <- replicateM (length p) [0, -1, 1]]

neighborCells :: Universe -> [Int] -> [([Int], Bool)]
neighborCells universe = map (\p -> (p, M.lookup p universe == Just True)) . neighbors
