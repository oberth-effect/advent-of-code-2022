import Data.List (sortOn)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Ord ( Down(Down) ) 

parseInput :: String -> Maybe [[Integer]]
parseInput = traverse (traverse readMaybe . lines) . splitOn "\n\n"

partOne :: [[Integer]] -> Integer
partOne = maximum . map sum

partTwo :: [[Integer]] -> Integer
partTwo = sum . take 3 . sortOn Down . map sum

main :: IO ()
main =
  do
    c <- getContents
    case parseInput c of
      Nothing -> do
        putStrLn "Error parsing input:"
      Just r -> do
        print $ partOne r
        print $ partTwo r
