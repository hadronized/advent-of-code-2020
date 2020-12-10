{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.List (foldl', scanl', sort)
import Numeric.Natural (Natural)
import Control.DeepSeq
import GHC.Generics (Generic)

main :: IO ()
main = do
    numbers :: [Int] <- fmap (sort . map read . lines) (readFile "input.txt")
    let (_, ones, threes) = part1 numbers
    putStrLn $ "Part 1: " <> show (ones * (threes + 1))
    let !t = trie 0 numbers
    print $ countTrie t
  where
    part1 = flip foldl' (0, 0, 0) diff
    diff (prev, ones, threes) n
      | n - prev == 1 = (n, ones + 1, threes)
      | n - prev == 3 = (n, ones, threes + 1)
      | otherwise = (n, ones, threes)

data Trie a = Trie !a ![Trie a] deriving (Eq, Show, Generic)

instance NFData a => NFData (Trie a)

trie :: Int -> [Int] -> Trie Int
trie n [] = Trie n []
trie n [x] = Trie n [trie x []]
trie n (a:b:xs)
    | b - n <= 3 = Trie n [subtree, t2]
    | otherwise = Trie n [subtree]
  where
    subtree = let x = trie a (b:xs) in deepseq x x
    t2 = let x = trie n (b:xs) in deepseq x x

countTrie :: Trie a -> Natural
countTrie (Trie !n []) = 1
countTrie (Trie !n !ts) = foldl' (\sum t -> sum + countTrie t) 0 ts

trieToString :: (Show a) => Trie a -> String
trieToString (Trie a l) = unlines (concatMap (trieToString_ [a]) l)

trieToString_ :: (Show a) => [a] -> Trie a -> [String]
trieToString_ h (Trie n []) = [show . reverse $ n : h]
trieToString_ h (Trie n l) = concatMap (trieToString_ (n : h)) l
