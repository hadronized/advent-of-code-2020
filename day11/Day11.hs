import Data.Vector (Vector, fromList)

main :: IO ()
main = do
  map <- (fromList . map fromList . unlines) <$> readFile "input.txt"
  print map
