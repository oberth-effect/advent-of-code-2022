module Day4 where

import           Common
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Elf = (Int, Int)

type Pair = (Elf, Elf)

type Parser = Parsec Void String

elf :: Parser Elf
elf = do
  a <- L.decimal
  char '-'
  b <- L.decimal
  return (a, b)

pair :: Parser Pair
pair = do
  a <- elf
  char ','
  b <- elf
  return (a, b)

parser :: Parser [Pair]
parser = pair `sepEndBy` eol

contain :: Elf -> Elf -> Bool
contain (a, b) (c, d)
  | a >= c && b <= d = True
  | a <= c && b >= d = True
  | otherwise = False

overlap :: Elf -> Elf -> Bool
overlap (a, b) (c, d)
  | a <= d && a >= c = True
  | b <= d && b >= c = True
  | c <= b && c >= a = True
  | d <= b && d >= a = True
  | otherwise = False

day4 :: Difficulty -> Problem [Pair] Int
day4 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . length . filter (uncurry contain)
        Hard -> Just . length . filter (uncurry overlap),
      printOutput = show
    }
