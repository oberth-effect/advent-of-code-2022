module Day6 where

import           Common
import           Data.List       (tails)
import           Data.List.Extra (anySame)

nelems :: Int -> String -> [String]
nelems n = map (take n) . tails

solver :: Int -> String -> Int
solver n s = n + length (takeWhile anySame (nelems n s))

day6 :: Difficulty -> Problem String Int
day6 diff =
  Problem
    { parseInput = Just . filter (/= '\n'),
      solve = case diff of
        Easy -> Just . solver 4
        Hard -> Just . solver 14,
      printOutput = show
    }
