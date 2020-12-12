module Main where

import Data.List (foldl', scanl')

main :: IO ()
main = do
  moves <- fmap (map parseMove . lines) (readFile "input.txt")
  let (_, x, y) = foldl' move ('E', 0, 0) moves
  print (abs x + abs y)
  let ((_, part2x, part2y), _) = part2 moves
  print (abs part2x + abs part2y)

part2 :: [(Char, Int)]-> ((Char, Int, Int), (Int,Int))
part2 = foldl' moveWithWaypoint (('E', 0, 0), (10, 1))
  where
    moveWithWaypoint (ship@(sd, sx, sy), waypoint) ('L', v) = (ship, rotate waypoint v)
    moveWithWaypoint (ship@(sd, sx, sy), waypoint) ('R', v) = (ship, rotate waypoint (360 - v))
    moveWithWaypoint ((sd, sx, sy), waypoint@(wx, wy)) ('F', v) = ((sd, sx + wx * v, sy + wy * v), waypoint)
    moveWithWaypoint (ship, (wx, wy)) (d, v) = let (_, wx', wy') = moveTowards d ('F', wx, wy) v in (ship, (wx', wy'))

parseMove :: String -> (Char, Int)
parseMove (verb:value) = (verb, read value)

move :: (Char, Int, Int) -> (Char, Int) -> (Char, Int, Int)
move (sd, sx, sy) ('L', v) = (turnLeft sd v, sx, sy)
move (sd, sx, sy) ('R', v) = (turnLeft sd (360 - v), sx, sy)
move ship@(sd, sx, sy) ('F', v) = moveTowards sd ship v
move ship (d, v) = moveTowards d ship v

turnLeft :: Char -> Int -> Char
turnLeft shipDir angle = dropWhile (/= shipDir) dirs !! (angle `div` 90)
  where
    dirs = cycle ['E', 'N', 'W', 'S']

moveTowards :: Char -> (Char, Int, Int) -> Int -> (Char, Int, Int)
moveTowards 'N' (sd, sx, sy) v = (sd, sx, sy + v)
moveTowards 'S' (sd, sx, sy) v = (sd, sx, sy - v)
moveTowards 'E' (sd, sx, sy) v = (sd, sx + v, sy)
moveTowards 'W' (sd, sx, sy) v = (sd, sx - v, sy)

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate (x, y) 0 = (x, y)
rotate (x, y) 90 = (-y, x)
rotate (x, y) 180 = (-x, -y)
rotate (x, y) 270 = (y, -x)
rotate (x, y) 360 = (x, y)
