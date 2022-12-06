module Day3 where

import Common
import Text.Read (readMaybe)
import Data.Char
import Data.List
import Data.List.Split (chunksOf)

priority :: Char -> Int
priority c
  | isLower c = fromEnum c - fromEnum 'a' + 1
  | otherwise = fromEnum c - fromEnum 'A' + 27

splitList :: [a] -> ([a], [a])
splitList l = splitAt (floor ( fromIntegral (length l) / 2)) l

linePriority :: String -> Int
linePriority = priority . head . uncurry intersect . splitList

listOfGroups :: [String] -> [[String]]
listOfGroups = chunksOf 3

stringListIntersect :: [String] -> String
stringListIntersect = foldr intersect (['a'..'z'] ++ ['A'.. 'Z'])

groupP :: [String] -> Int
groupP =  sum . map (priority . head . stringListIntersect) . listOfGroups

day3 :: Difficulty -> Problem [String] Int
day3 diff =
  Problem
    { parseInput = Just . lines,
      solve = case diff of
        Easy -> Just . sum . map linePriority
        Hard -> Just . groupP,
      printOutput = show
    }

