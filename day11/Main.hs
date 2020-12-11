module Main where

import Data.Maybe (catMaybes)
import Data.Vector (Vector, fromList, (!), (!?), (//), imap)
import qualified Data.Vector as V (sum)

main :: IO ()
main = do
  map <- fromList . map fromList . lines <$> readFile "input.txt"
  print $ solve <$> [golRule1, golRule2] <*> pure map

part1 :: Vector (Vector Char) -> Int
part1 = solve golRule1

solve :: Rule -> Vector (Vector Char) -> Int
solve rule = go
  where
    go map =
      let newMap = mutateMap rule map
      in if newMap == map then countOccupied newMap else go newMap
    countOccupied = V.sum . fmap (V.sum . fmap isOccupied)

mutateMap :: Rule -> Vector (Vector Char) -> Vector (Vector Char)
mutateMap rule map = imap mutateRow map
  where
    mutateRow y = imap (mutateCol y)
    mutateCol y x = rule x y map

type Rule = Int -> Int -> Vector (Vector Char) -> Char -> Char

golRule1 :: Rule
golRule1 x y map cell =
    case cell of
      'L' -> if adjacentSeats == 0 then '#' else 'L'
      '#' -> if adjacentSeats >= 4 then 'L' else '#'
      _ -> cell
  where
    adjacentSeats = sum (catMaybes seats)
    seats = [isOccupied <$> getCell (x + x') (y + y') | x' <- [-1..1], y' <- [-1..1], x' /= 0 || y' /= 0]
    getCell :: Int -> Int -> Maybe Char
    getCell x y = do
      row <- map !? y
      row !? x

golRule2 :: Rule
golRule2 x y map cell =
    case cell of
      'L' -> if visibleSeats == 0 then '#' else 'L'
      '#' -> if visibleSeats >= 5 then 'L' else '#'
      _ -> cell
  where
    visibleSeats = sum (catMaybes seats)
    seats = [isOccupied <$> coFind (x + dx, y + dy) (dx, dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
    getCell :: Int -> Int -> Maybe Char
    getCell x y = do
      row <- map !? y
      row !? x
    coFind (x, y) dir@(dx, dy) = do
      c <- getCell x y
      if c == 'L' || c == '#' then Just c else coFind (x + dx, y + dy) dir

isOccupied :: Char -> Int
isOccupied '#' = 1
isOccupied _ = 0
