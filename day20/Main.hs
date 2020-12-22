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
import Data.Foldable (find, foldl', traverse_)
import Data.List (groupBy, unfoldr)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M
import qualified Data.Vector as V

data Piece = Piece {
    pieceID :: Int
  , pieceBorders :: [Int]
  , pieceParts :: Parts
  } deriving (Eq, Show)

type Parts = [[Char]]

main :: IO ()
main = do
    pieces <- parsePuzzle <$> readFile "input.txt"
    let connectivity = connectedPieces pieces
        corners = map snd . M.toList . flip M.filter pieces $ \piece -> length (filter ((== 2) . length . fromMaybe [] . flip M.lookup connectivity) $ pieceBorders piece) == 2
        part1 = product (map pieceID corners)
    putStrLn $ "Part 1: " <> show part1

    let topLeft = topLeftPiece connectivity corners
        puzzle = buildPuzzle topLeft pieces connectivity
        vpuzzle = vectorPuzzle puzzle

    print $ V.length vpuzzle
    print $ length $ puzzle
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

northi, easti, southi, westi :: Int
northi = 0
easti = 1
southi = 2
westi = 3

-- | Extract all borders of a piece in the following order: [N, E, S, W].
extractBorders :: Parts -> Parts
extractBorders parts = [north, east, south, west]
  where
    north = extractNorthBorder parts
    east = extractEastBorder parts
    south = extractSouthBorder parts
    west = extractWestBorder parts

extractNorthBorder :: Parts -> [Char]
extractNorthBorder = head

extractSouthBorder :: Parts -> [Char]
extractSouthBorder = last

extractWestBorder :: Parts -> [Char]
extractWestBorder = map head

extractEastBorder :: Parts -> [Char]
extractEastBorder = map last

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
topLeftPiece connectivity corners = fromJust . flip find corners $ \piece ->
  let borders = pieceBorders piece
      eastSouthOnly = do
        north <- M.lookup (borders !! northi) connectivity
        west <- M.lookup (borders !! westi) connectivity
        south <- M.lookup (borders !! southi) connectivity
        east <- M.lookup (borders !! easti) connectivity

        pure $ length north == 1 && length west == 1 && length east == 2 && length south == 2
  in eastSouthOnly == Just True

-- Build the puzzle (list version)
buildPuzzle :: Piece -> Map Int Piece -> Map Int [Int] -> [[Parts]]
buildPuzzle topLeft pieces connectivity = buildSouth topLeft
  where
    buildSouth piece =
      let southPieceIDs = filter (/= pieceID piece) . fromJust $ M.lookup (pieceBorders piece !! southi) connectivity
          [southPieceID] = southPieceIDs
      in unfoldr buildEast (Just piece) : if null southPieceIDs then [] else buildSouth (correctedSouthPiece piece . fromJust $ M.lookup southPieceID pieces)
    buildEast Nothing = Nothing
    buildEast (Just piece) =
      let eastPieceIDs = filter (/= pieceID piece) . fromJust $ M.lookup (pieceBorders piece !! easti) connectivity
          [eastPieceID] = eastPieceIDs
      in if null eastPieceIDs then Just (pieceParts piece, Nothing) else Just (pieceParts piece, fmap (correctedEastPiece piece) $ M.lookup eastPieceID pieces)
    correctedEastPiece = transformUntilMatch extractEastBorder extractWestBorder
    correctedSouthPiece = transformUntilMatch extractSouthBorder extractNorthBorder

-- | Transform a piece’s parts until it matches the border of the first part.
transformUntilMatch :: (Parts -> [Char]) -> (Parts -> [Char]) -> Piece -> Piece -> Piece
transformUntilMatch truth project refPiece victim = victim { pieceParts = parts, pieceBorders = borders }
  where
    parts = fromJust . find ((== refBorder) . project) $ transforms <*> [pieceParts victim, hflip $ pieceParts victim]
    borders = map borderToInt (extractBorders parts)
    refBorder = reverse (truth $ pieceParts refPiece)
    transforms = [id, clockWiseRot90, times 2 clockWiseRot90, times 3 clockWiseRot90]

-- | Apply a function N times (similar to stimes for endofunctors).
times :: Int -> (a -> a) -> a -> a
times 1 f = f
times n f = f . times (n - 1) f

-- | Build the Vector representation of a puzzle.
vectorPuzzle :: [[Parts]] -> V.Vector Char
vectorPuzzle = V.fromList . concat . concatMap (glueParts . map removeBorders)
  where
    glueParts :: [Parts] -> Parts
    glueParts (([]:_):_) = []
    glueParts blocks = concatMap (map head) blocks : glueParts (map (map tail) blocks)
    removeBorders :: Parts -> Parts
    removeBorders = map (tail . init) . tail . init
