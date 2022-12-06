module Day2 where

import Common
import Control.Monad
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Read (readMaybe)

data RPS = Rock | Paper | Scissors deriving (Show, Enum)

next :: RPS -> RPS
next Scissors = Rock
next a = succ a

prev :: RPS -> RPS
prev Rock = Scissors
prev a = pred a

parseOppo :: String -> Maybe RPS
parseOppo "A" = Just Rock
parseOppo "B" = Just Paper
parseOppo "C" = Just Scissors
parseOppo _ = Nothing

parseMine :: String -> Maybe RPS
parseMine "X" = Just Rock
parseMine "Y" = Just Paper
parseMine "Z" = Just Scissors
parseMine _ = Nothing

listToPair :: [a] -> Maybe (a, a)
listToPair [] = Nothing
listToPair [x] = Nothing
listToPair (x : xs) = Just (x, head xs)

parseLines :: String -> Maybe [(String, String)]
parseLines = traverse (listToPair . words) . lines

parser :: String -> Maybe [(RPS, RPS)]
parser = parseLines >=> traverse match

match :: (String, String) -> Maybe (RPS, RPS)
match = bitraverse parseOppo parseMine

score :: (RPS, RPS) -> Int
score (x, y) = (fromEnum y + 1) + ((fromEnum y - fromEnum x + 1) `mod` 3) * 3

score2 :: (RPS, RPS) -> Int
score2 (x, y) = case y of
  Rock -> (fromEnum $ prev x) + 1
  Paper -> (fromEnum x) + 1 + 3
  Scissors -> (fromEnum $ next x) + 1 + 6

day2 :: Difficulty -> Problem [(RPS, RPS)] Int
day2 diff =
  Problem
    { parseInput = parser,
      solve = case diff of
        Easy -> Just . sum . map score
        Hard -> Just . sum . map score2,
      printOutput = show
    }
