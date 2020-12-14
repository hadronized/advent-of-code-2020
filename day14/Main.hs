module Main where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.Map as M (Map, elems, empty, insert)

main :: IO ()
main = do
    program <- map parseInstr . lines <$> readFile "input.txt"
    putStrLn $ "Part 1: " <> show (execute (\a v m -> [(a, mask v m)]) program)
    putStrLn $ "Part 2: " <> show (execute (\a v m -> fmap (\m' -> (mask a m', v)) $ maskPerm m) program)
  where
    parseInstr line = case break (== '=') line of
      ("mask ", r) -> String (drop 2 r)
      (_, r) ->
        let addr = read . takeWhile (/= ']') $ drop 4 line
            value = read (drop 2 r)
        in Write addr value

execute :: (Int64 -> Int64 -> String -> [(Int64, Int64)]) -> [Instr] -> Int64
execute extractValues = sum . elems . fst . foldl' executeInstr (empty, replicate 36 'X')
  where
    executeInstr (memory, _) (String m) = (memory, m)
    executeInstr (memory, m) (Write addr value) =
      let writes = extractValues addr value m
          memory' = foldl' (\mem (a, v) -> insert a v mem) memory writes
      in (memory', m)

data Instr = String String | Write Int64 Int64 deriving (Show)

parseNumber :: String -> Int64
parseNumber = go 0
  where
    go n [] = n
    go n ('0':bs) = go (n `shiftL` 1) bs
    go n ('1':bs) = go ((n `shiftL` 1) .|. 1) bs

mask :: Int64 -> String -> Int64
mask i m = (i .|. orString 0 m) .&. andString 0 m
  where
    orString n [] = n
    orString n ('1':bs) = orString ((n `shiftL` 1) .|. 1) bs
    orString n (_:bs) = orString (n `shiftL` 1) bs
    andString n [] = n
    andString n ('X':bs) = andString ((n `shiftL` 1) .|. 1) bs
    andString n ('1':bs) = andString ((n `shiftL` 1) .|. 1) bs
    andString n (_:bs) = andString (n `shiftL` 1) bs

maskPerm :: String -> [String]
maskPerm =  go []
  where
    go m [] = [reverse m]
    go m ('X':bs) = go ('0' : m) bs ++ go ('1' : m) bs
    go m ('0':bs) = go ('X' : m) bs
    go m ('1':bs) = go ('1' : m) bs
