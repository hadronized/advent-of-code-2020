module Main where

import Data.Bifunctor (first)
import Data.Char (isNumber, isSpace)

main :: IO ()
main = do
    passports <- fmap (map parse . lines) (readFile "input.txt")
    print . length $ filter verifyPart1 passports
    print . length $ filter verifyPart2 passports
  where
    parse :: String -> (Int, Int, Char, String)
    parse line = (mn, mx, l, h)
      where
        (mn, mns) = first read (span isNumber line)
        (mx, mxs) = first read (span isNumber $ tail mns)
        (l, ls) = first head $ break (== ':') (tail mxs)
        h = drop 2 ls
    verifyPart1 (mn, mx, l, h) = mn <= s && s <= mx
      where s = length $ filter (== l) h
    verifyPart2 (mn, mx, l, h) = (h !! (mn - 1) == l) /= (h !! (mx - 1) == l)
