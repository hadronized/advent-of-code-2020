module Main where

import Data.List (groupBy)
import Data.Function (on)

data Piece = Piece {
    pieceID :: Int
  , pieceParts :: [[Char]]
  } deriving (Eq, Show)

main :: IO ()
main = do
    puzzle <- parsePuzzle <$> readFile "input.txt"
    print puzzle
  where
    parsePuzzle = map parsePiece . filter (not . null . head) . groupBy ((&&) `on` not . null) . lines
    parsePiece (title:parts) =
      let pid = read . takeWhile (/= ':') $ drop (length "Title") title
      in Piece pid parts

-- Vertical flip (do a kickflip!)
--
-- a b c    c b a
-- d e f -> f e d
-- g h i    i h g
vflip :: [[a]] -> [[a]]
vflip = map reverse

-- Horizontal flip.
-- a b c    g h i
-- d e f -> d e f
-- g h i    a b c
hflip :: [[a]] -> [[a]]
hflip = reverse

-- 90° rotation (clock-wise)
clockWiseRot90 :: [[a]] -> [[a]]
clockWiseRot90 ([]:_) = []
clockWiseRot90 l = reverse (map head l) : clockWiseRot90 (map tail l)
