module Main where

import Data.Function (on)
import Data.Foldable (foldl')
import Data.List (groupBy)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

data Piece = Piece {
    pieceID :: Int
  , pieceBorders :: [Int]
  , pieceParts :: [[Char]]
  } deriving (Eq, Show)

main :: IO ()
main = do
    pieces <- parsePuzzle <$> readFile "input.txt"
    let connected = connectedPieces pieces
        sortedPieces = flip M.filter pieces $ \piece -> length (filter ((== 2) . length . fromMaybe [] . flip M.lookup connected) $ pieceBorders piece) == 2
        part1 = product . map (pieceID . snd) $ M.toList sortedPieces
    putStrLn $ "Part 1: " <> show part1
  where
    parsePuzzle = M.fromList . map parsePiece . filter (not . null . head) . groupBy ((&&) `on` not . null) . lines
    parsePiece (title:parts) =
      let pid = read . takeWhile (/= ':') $ drop (length "Title") title
          borders = map borderToInt (extractBorders parts)
      in (pid, Piece pid borders parts)

-- | Vertical flip (do a kickflip!)
--
-- a b c    c b a
-- d e f -> f e d
-- g h i    i h g
vflip :: [[a]] -> [[a]]
vflip = map reverse

-- | Horizontal flip.
-- a b c    g h i
-- d e f -> d e f
-- g h i    a b c
hflip :: [[a]] -> [[a]]
hflip = reverse

-- | 90° rotation (clock-wise)
clockWiseRot90 :: [[a]] -> [[a]]
clockWiseRot90 ([]:_) = []
clockWiseRot90 l = reverse (map head l) : clockWiseRot90 (map tail l)

-- | Extract all borders of a piece in the following order: [N, W, S, E].
extractBorders :: [[Char]] -> [[Char]]
extractBorders parts = [north, west, south, east]
  where
    north = head parts
    south = last parts
    west = map last parts
    east = map head parts

-- | Switch from a raw border representation to a numeric representation.
borderToInt :: [Char] -> Int
borderToInt b = compute b `min` compute (reverse b)
  where
    compute = flip foldl' 0 $ \n c -> case c of
      '.' -> n * 2
      '#' -> n * 2 + 1

-- | Match pieces to each other based on their borders. The map shows for a given border
-- all (at max two) the pieces sharing it.
connectedPieces :: Map Int Piece -> Map Int [Int]
connectedPieces = foldl' f M.empty . M.toList
  where
    f connected (pid, piece) = foldl' f' connected (pieceBorders piece)
      where
        f' connected' border = M.insertWith (<>) border [pid] connected'
