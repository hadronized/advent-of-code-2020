module Main where

import Data.Bifunctor (Bifunctor(..))
import Data.Int (Int64)
import Data.IntSet as S (delete, fromList, size, toList)
import Data.List (find, foldl', isPrefixOf, groupBy, sortBy)
import Data.Maybe (mapMaybe, isNothing)
import Data.Ord (comparing)
import Data.Vector as V ((!), findIndex, imap, replicate)

main :: IO ()
main = do
  notes <- parseNotes <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (part1 notes)
  putStrLn $ "Part 2: " <> show (part2 notes)

data Notes = Notes {
    getClasses :: [(String, (Int, Int), (Int, Int))]
  , myTicket :: [Int]
  , nearbyTickets :: [[Int]]
} deriving (Eq, Show)

parseNotes :: String -> Notes
parseNotes raw = Notes classes myTicket nearbyTickets
  where
    (rawClasses, myTicketStr:nearbyTicketStr) = second (drop 1) . break (== "your ticket:") $ filter (not . null) $ lines raw
    classes = map parseClass rawClasses
    myTicket = parseTicket myTicketStr
    nearbyTickets = map parseTicket $ tail nearbyTicketStr
    parseClass line = (className, lowerRule, upperRule)
      where
        (className, pairs) = second (drop 2) $ break (== ':') line
        [lower, _, upper] = words pairs
        (lowerRule, upperRule) = (parsePair lower, parsePair upper)
        parsePair = bimap read (read . drop 1) . break (== '-')
    parseTicket = map read . filter (/= ",") . groupBy (\a b -> a /= ',' && b /= ',')

part1 :: Notes -> Int
part1 notes = sum . mapMaybe (invalid unnamedRules) $ nearbyTickets notes
  where
    unnamedRules = map (\(_, a, b) -> (a, b)) (getClasses notes)

part2 :: Notes -> Int64
part2 notes = departures
  where
    orders = findOrders notes
    classes = map (\(n, _, _) -> n) (getClasses notes)
    reorderedClasses = [classes !! order | order <- orders]
    myFixedTicket = zip reorderedClasses (myTicket notes)
    departures = product . map (fromIntegral . snd) . filter (isPrefixOf "departure" . fst) $ myFixedTicket

findOrders :: Notes -> [Int]
findOrders notes = reduceSet [] $ foldl' (\sets ticket -> updateFields sets $ zip [0..] ticket) fields tickets
  where
    tickets = filter (isNothing . invalid unnamedRules) $ nearbyTickets notes
    unnamedRules = map (\(_, a, b) -> (a, b)) (getClasses notes)
    fieldsNb = length (head tickets)
    allFieldsSet = S.fromList [ 0 .. fieldsNb - 1]
    fields = V.replicate fieldsNb allFieldsSet
    updateFields sets [] = sets
    updateFields sets ((fi, f):fs) = updateFields (imap updateField sets) fs
      where
        rulesValidities = map (uncurry $ checkFieldValidity f) unnamedRules
        updateField si set
          | rulesValidities !! si = set
          | otherwise = S.delete fi set
    reduceSet known sets = case V.findIndex ((== 1) . size) sets of
      Just setIndex ->
        let [okay] = S.toList $ sets ! setIndex
            known' = (setIndex, okay) : known
            sets' = fmap (S.delete okay) sets
        in reduceSet known' sets'
      Nothing -> map fst $ sortBy (comparing snd) known

checkFieldValidity :: Int -> (Int, Int) -> (Int, Int) -> Bool
checkFieldValidity f (la, ua) (lb, ub) = (la <= f && f <= ua) || (lb <= f && f <= ub)

invalid :: [((Int, Int), (Int, Int))] -> [Int] -> Maybe Int
invalid unnamedRules = find $ \f -> all (\r -> not $ uncurry (checkFieldValidity f) r) unnamedRules
