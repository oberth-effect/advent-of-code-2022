module Day1 where

import Common
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))
import Text.Read (readMaybe)

partOne :: [[Integer]] -> Integer
partOne = maximum . map sum

partTwo :: [[Integer]] -> Integer
partTwo = sum . take 3 . sortOn Down . map sum

day1 :: Difficulty -> Problem [[Integer]] Integer
day1 diff =
  Problem
    { parseInput = traverse (traverse readMaybe . lines) . splitOn "\n\n"
      ,solve = case diff of
        Easy -> Just . partOne
        Hard -> Just . partTwo
      ,printOutput = show
      }

  
