module Day1 where

import Base
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))
import Text.Read (readMaybe)

partOne :: [[Integer]] -> Integer
partOne = maximum . map sum

partTwo :: [[Integer]] -> Integer
partTwo = sum . take 3 . sortOn Down . map sum

day1 :: Problem [[Integer]] Integer
day1 =
  Problem
    { parseInput = traverse (traverse readMaybe . lines) . splitOn "\n\n",
      solve = \diff -> case diff of
        Easy -> partOne
        Hard -> partTwo
    }

main :: IO ()
main = aocRun day1
