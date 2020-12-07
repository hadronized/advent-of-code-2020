module Main

import Data.List
import Data.Strings
import System.File

fromRight : a -> Either _ a -> a
fromRight _ (Right x) = x
fromRight x _ = x

fromJust : a -> Maybe a -> a
fromJust _ (Just x) = x
fromJust x _ = x

main : IO ()
main = do
    numbers <- map (parse . fromRight "") $ readFile "input.txt"
    let part1 = [a * b | x <- tails numbers, y <- tails x, (a, b) <- zip x y, a + b == 2020]
    let part2 = [a * b * c | x <- tails numbers, y <- tails x, z <- tails y, (a, b, c) <- zip3 x y z, a + b + c == 2020]
    putStrLn $ "Part 1: " ++ show (take 1 part1)
    putStrLn $ "Part 2: " ++ show (take 1 part2)
  where
    parse : String -> List Int
    parse = map (fromJust 0 . parsePositive) . lines
