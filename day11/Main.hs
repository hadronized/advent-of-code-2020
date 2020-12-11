module Main where

import Data.Maybe (catMaybes)
import Data.Vector (Vector, fromList, (!), (!?), imap)
import qualified Data.Vector as V (sum)

main :: IO ()
main = do
    map <- fromList . map fromList . lines <$> readFile "input.txt"
    print $ solve <$> [rule1, rule2] <*> pure map
  where
    rule1 = genRule 4 $ \getCell x y dx dy  -> getCell (x + dx) (y + dy)
    rule2 = genRule 5 coFind
      where
        coFind getCell x' y' dx' dy' = go (x' + dx') (y' + dy') dx' dy'
          where
            go x y dx dy = do
              c <- getCell x y
              if c == 'L' || c == '#' then Just c else go (x + dx) (y + dy) dx dy

type Rule = Int -> Int -> Vector (Vector Char) -> Char -> Char

solve :: Rule -> Vector (Vector Char) -> Int
solve rule = go
  where
    go map =
      let newMap = mutateMap rule map
      in if newMap == map then countOccupied newMap else go newMap
    countOccupied = V.sum . fmap (V.sum . fmap isOccupied)
    mutateMap rule map = imap mutateRow map
      where
        mutateRow y = imap (mutateCol y)
        mutateCol y x = rule x y map

genRule :: Int -> ((Int -> Int -> Maybe Char) -> Int -> Int -> Int -> Int -> Maybe Char) -> Rule
genRule overcrowded findCell x y map cell =
    case cell of
      'L' -> if adjacentSeats == 0 then '#' else 'L'
      '#' -> if adjacentSeats >= overcrowded then 'L' else '#'
      _ -> cell
  where
    adjacentSeats = sum (catMaybes seats)
    seats = [isOccupied <$> findCell getCell x y x' y' | x' <- [-1..1], y' <- [-1..1], x' /= 0 || y' /= 0]
    getCell :: Int -> Int -> Maybe Char
    getCell x y = do
      row <- map !? y
      row !? x

isOccupied :: Char -> Int
isOccupied '#' = 1
isOccupied _ = 0
