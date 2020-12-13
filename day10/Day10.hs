import Data.List (foldl', sort)
import Data.Map as M (Map, empty, insert, lookup)

main :: IO ()
main = do
    numbers <- fmap (addLast3 . sort . map read . lines) (readFile "input.txt")
    let (_, ones, threes) = part1 numbers
    putStrLn $ "Part 1: " <> show (ones * threes)
    putStrLn $ "Part 2: " <> show (part2 numbers)
  where
    addLast3 [x] = [x, x + 3]
    addLast3 (x:xs) = x : addLast3 xs
    part1 = flip foldl' (0, 0, 0) diff
    diff (prev, ones, threes) n
      | n - prev == 1 = (n, ones + 1, threes)
      | n - prev == 3 = (n, ones, threes + 1)
      | otherwise = (n, ones, threes)

unsafeFromJust :: Maybe a -> a
unsafeFromJust (Just a) = a
unsafeFromJust Nothing = error "unsafeFromJust Nothing"

part2 :: [Int] -> Map [Int] Int
part2 = go empty []
  where
    go :: Map [Int] Int -> [Int] -> [Int] -> Map [Int] Int
    go memo pl l@[x, y] = M.insert l 1 memo
    go memo pl l@(a:b:c:xs)
        | c - b <= 3 = let memo'' = go' in insert l (1 + unsafeFromJust (M.lookup (b:c:xs) memo'')) memo''
        | otherwise = insert l next memo'
      where
        go' = go memo' [] (reverse pl ++ l)
        l = b:c:xs
        memo' = go memo (a:pl) l
        next = unsafeFromJust $ M.lookup l memo'
