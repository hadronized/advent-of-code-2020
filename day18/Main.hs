module Main where

import Data.Char (digitToInt, isNumber)

main :: IO ()
main = do
  lns <- lines <$> readFile "input.txt"
  let part1 = sum $ map (\l -> let [Number x] = parseExpr False l in x) lns
  let part2 = sum $ map (\l -> let [Number x] = parseExpr True l in x) lns
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2

data Token = NewExpr | Op Char | Number Int deriving (Eq, Show)

parseExpr :: Bool -> String -> [Token]
parseExpr prec' = parse prec' [] 0
  where
    parse _ stack n [] = reduceStack False False $ reduceStack False True $ Number n : stack
    parse prec stack n (' ':s) = parse prec stack n s
    parse prec stack n ('(':s) = parse prec (NewExpr : stack) n s
    parse prec stack n (')':s) = parse prec (reduceStack True False $ Number n : stack) 0 s
    parse prec stack n (c:s)
      | isNumber c = parse prec stack (n * 10 + digitToInt c) s
      | otherwise = parse prec (Op c : reduceStack False prec (Number n : stack)) 0 s
    reduceStack _ _ [] = []
    reduceStack True prec (Number n : NewExpr : s) = Number n : s
    reduceStack parens prec (Number _ : r@(Number _ : _)) = reduceStack parens prec r
    reduceStack parens False (Number a : Op op : Number b : s) = reduceStack parens False $ Number (applyOp op b a) : s
    reduceStack parens True (Number a : Op op : Number b : s)
      | op == '+' = reduceStack parens True $ Number (applyOp op b a) : s
      | otherwise = Number a : Op op : reduceStack parens True (Number b : s)
    reduceStack _ _ s = s
    applyOp '+' = (+)
    applyOp '-' = (-)
    applyOp '*' = (*)
