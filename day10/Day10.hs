{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (foldl', scanl', sort)
import Numeric.Natural (Natural)
import qualified Data.Map as M
import Control.Monad.State (State(..), get)

main :: IO ()
main = do
    numbers :: [Int] <- fmap (sort . map read . lines) (readFile "input.txt")
    let (_, ones, threes) = part1 numbers
    putStrLn $ "Part 1: " <> show (ones * (threes + 1))
    -- let t = trie M.empty 0 numbers
    -- print (countTrie t)
  where
    part1 = flip foldl' (0, 0, 0) diff
    diff (prev, ones, threes) n
      | n - prev == 1 = (n, ones + 1, threes)
      | n - prev == 3 = (n, ones, threes + 1)
      | otherwise = (n, ones, threes)

data Trie a = Trie a [Trie a] deriving (Eq, Show)

trie :: Int -> [Int] -> State (M.Map [Int] (Trie Int)) (Trie Int)
trie _ [] = error "impossible"
trie _ [x] = pure $ Trie x []
trie n l@(a:b:xs) = do
    memo <- get
    case M.lookup l memo of
      Just t -> pure t
      Nothing -> do
        let children = if b - n <= 3 then [subtree, trie memo n (b:xs)] else [subtree]
        storeAndPure memo l (Trie a children)
  where
    subtree = trie memo a (b:xs)
    storeAndPure memo l t = do
      set (M.insert l t)
      pure t

countTrie :: Trie a -> Natural
countTrie (Trie n []) = 1
countTrie (Trie n ts) = sum (map countTrie ts)

trieToString :: (Show a) => Trie a -> String
trieToString (Trie a l) = unlines (concatMap (trieToString_ [a]) l)

trieToString_ :: (Show a) => [a] -> Trie a -> [String]
trieToString_ h (Trie n []) = [show . reverse $ n : h]
trieToString_ h (Trie n l) = concatMap (trieToString_ (n : h)) l
