module Main where

import Data.Foldable (foldl')
import Data.Map as M (Map, empty, foldlWithKey, insert, insertWith, lookup, mapWithKey)

type Universe3 = Map (Int, Int, Int) Bool
type Universe4 = Map (Int, Int, Int, Int) Bool

main :: IO ()
main = do
    universe <- parseUniverse3 <$> readFile "input.txt" -- I have no shame
    universe' <- parseUniverse4 <$> readFile "input.txt" -- lol
    let universe6 = iterate step3 universe !! 6
    let universe6' = iterate step4 universe' !! 6
    putStrLn $ "Part 1: " <> show (countAlive universe6)
    putStrLn $ "Part 2: " <> show (countAlive universe6')
  where
    parseUniverse3 = foldl' parseSlice3 empty . zip [0..] . lines
    parseSlice3 universe (y, line) = foldl' parseCell universe $ zip [0..] line
      where
        parseCell universe (x, '#') = insert (x, y, 0) True universe
        parseCell universe (x, '.') = insert (x, y, 0) False universe
    parseUniverse4 = foldl' parseSlice4 empty . zip [0..] . lines
    parseSlice4 universe (y, line) = foldl' parseCell universe $ zip [0..] line
      where
        parseCell universe (x, '#') = insert (x, y, 0, 0) True universe
        parseCell universe (x, '.') = insert (x, y, 0, 0) False universe

step3 :: Universe3 -> Universe3
step3 = mutate . inflate
  where
    -- inflate a universe; what it means is that for a given universe, it will automatically generate void cells on its
    -- border
    inflate = flip foldlWithKey empty $ \newUniverse p cellActive ->
      if cellActive
      then
        insert p True $ foldl' (\universe (x, y, z) -> insertWith (const id) (x, y, z) False universe) newUniverse (neighbors3 p)
      else
        newUniverse
    -- mutate a universe
    mutate universe = flip mapWithKey universe $ \p cellActive ->
      let aliveCount = length . filter (\(_, _, _, a) -> a) $ neighborCells3 universe p
      in if cellActive then aliveCount == 2 || aliveCount == 3 else aliveCount == 3
    neighbors3 (a, b, c) = [(x + a, y + b, z + c) | x <- [-1..1], y <- [-1..1], z <- [-1..1], x /= 0 || y /= 0 || z /= 0]
    neighborCells3 universe = map (\p@(x, y, z) -> (x, y, z, M.lookup p universe == Just True)) . neighbors3

step4 :: Universe4 -> Universe4
step4 = mutate . inflate
  where
    -- inflate a universe; what it means is that for a given universe, it will automatically generate void cells on its
    -- border
    inflate = flip foldlWithKey empty $ \newUniverse p cellActive ->
      if cellActive
      then
        insert p True $ foldl' (\universe (x, y, z, w) -> insertWith (const id) (x, y, z, w) False universe) newUniverse (neighbors4 p)
      else
        newUniverse
    -- mutate a universe
    mutate universe = flip mapWithKey universe $ \p cellActive ->
      let aliveCount = length . filter (\(_, _, _, _, a) -> a) $ neighborCells4 universe p
      in if cellActive then aliveCount == 2 || aliveCount == 3 else aliveCount == 3
    neighbors4 (a, b, c, d) = [(x + a, y + b, z + c, w + d) | x <- [-1..1], y <- [-1..1], z <- [-1..1], w <- [-1..1], x /= 0 || y /= 0 || z /= 0 || w /= 0]
    neighborCells4 universe = map (\p@(x, y, z, w) -> (x, y, z, w, M.lookup p universe == Just True)) . neighbors4

countAlive :: (Functor f, Foldable f, Enum a) => f a -> Int
countAlive = sum . fmap fromEnum
