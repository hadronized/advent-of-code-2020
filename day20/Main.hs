-- I’m writing a bunch of stuff here so that I can think.
--
-- The first part of the puzzle was pretty easy: we need to find the four corners (their IDs) and multiply them. This is
-- easy because the four corners are the _only_ pieces that share only two borders with other pieces (the other two
-- corners are not shared with any other pieces). The way I did this was to add some metadata to piece at parsing time: I
-- just add their borders has a list of [north, west, south, east], where the items are hashes of the borders. A border is
-- just a 10-element list of either '.' or '#'. I considered '.' to be a 0-bit and '#' a 1-bit. The hash is then simply
-- considering those lists as 10-bit integers. However, because we need to be able to make puzzle pieces connect, two
-- different hashes can be compatible. For this reason, I wrote my hash function in a way that if two borders are “the
-- same flipped”, their hashes is the same too. The idea is simply:
--
--   hash border = binaryHash border `min` binaryHash (reverse border)
--
-- For the second part, it’s much trickier. The algorithm I want to write will automatically fill the “final” list of
-- rendered things. For this reason, I need to start at top-left, put all pieces to the right and I can’t go on anymore,
-- go to the south of the first line, and iterate until I can’t go south anymore. Once I have everything, I can simply
-- look for monsters. If I find none, I rotate the whole thing once, and I look for them once again. 3 rotations are
-- needed needed first. If we still haven’t found any monsters, we can flip the whole thing (horizontal flip is faster)
-- and go on with the rotations.

module Main where

import Data.Function (on)
import Data.Foldable (find, foldl')
import Data.List (groupBy)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
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
        corners = flip M.filter pieces $ \piece -> length (filter ((== 2) . length . fromMaybe [] . flip M.lookup connected) $ pieceBorders piece) == 2
        part1 = product . map (pieceID . snd) $ M.toList corners
    putStrLn $ "Part 1: " <> show part1
    print corners
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

northi, westi, southi, easti :: Int
northi = 0
westi = 1
southi = 2
easti = 3

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

-- | Find the top-left most piece.
topLeftPiece :: Map Int [Int] -> [Piece] -> Piece
topLeftPiece connectivity pieces = fromJust . flip find pieces $ \piece ->
  let borders = pieceBorders piece
      westSouthOnly = do
        north <- M.lookup (borders !! northi) connectivity
        west <- M.lookup (borders !! westi) connectivity
        south <- M.lookup (borders !! southi) connectivity
        east <- M.lookup (borders !! easti) connectivity

        pure $ length north == 1 && length east == 1 && length west == 2 && length south == 2
  in westSouthOnly == Just True
