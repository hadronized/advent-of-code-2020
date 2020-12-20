module Main where

import Control.Arrow (second)
import Data.Bifunctor (bimap)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.IntSet as S (delete, fromList, insert, member)
import Data.Vector as V (Vector, (!), (//), fromList)

data Rule = SingleChar Char | SeqRule [[Int]] deriving (Eq, Show)

type Rules = Vector Rule

main :: IO ()
main = do
    (rules, msgs) <- parseLeStuff <$> readFile "input.txt"
    putStrLn $ "Part 1: " <> show (length $ filter (isMsgValid rules) msgs)
    let rules2 = rules // [(8, SeqRule [[42], [42, 8]]), (11, SeqRule [[42, 31], [42, 11, 31]])]
    putStrLn $ "Part 2: " <> show (length $ filter (isMsgValid rules) msgs)
  where
    parseLeStuff = bimap (V.fromList . map snd . sortBy (comparing fst) . map parseRule) tail . break null . lines
    parseRule :: String -> (Int, Rule)
    parseRule line = (read ruleID, rule)
      where
        (ruleID, ruleIDRest) = second (drop 2) $ break (== ':') line
        rule = case head ruleIDRest of
          '\"' -> SingleChar (ruleIDRest !! 1)
          _ -> SeqRule . map (map read) . filter (/= ["|"]) . groupBy (\a b -> a /= "|" && b /= "|") $ words ruleIDRest

data RuleZipper = RuleZipper {
    parentRules :: [(Int, Rule)]
  , currentID :: Int
  , currentRule :: Rule
  } deriving (Eq, Show)

isMsgValid :: Rules -> String -> Bool
isMsgValid rules = go (S.fromList []) (RuleZipper [] 0 $ rules ! 0)
  where
    go seen (RuleZipper [] _ (SingleChar c)) [x] = c == x
    go seen (RuleZipper ((parentID, parent):ps) _ (SingleChar c)) (x:xs)
      | x == c = go (delete parentID seen) (RuleZipper ps parentID parent) xs
      | otherwise = False
    go seen (RuleZipper parents ruleID (SeqRule seqs)) l@(x:xs)
      | ruleID `member` seen = False
      | otherwise = any (\ids -> let ((newRuleID, newRule):newParents) = map (\i -> (i, rules ! i)) ids <> parents in go (insert ruleID seen) (RuleZipper newParents newRuleID newRule) l) seqs
    go seen _ _ = False
