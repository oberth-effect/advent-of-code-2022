{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import           Common
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Instruction = Instruction
  { icount :: Int,
    ifrom  :: Int,
    ito    :: Int
  }
  deriving (Eq, Show, Ord)

type Crate = Char

type CharParser a = Parsec Void String a

withNewLines :: CharParser a -> CharParser [a]
withNewLines = flip sepEndBy newline

parseCrate :: CharParser (Maybe Crate)
parseCrate = (Just <$> crate) <|> (Nothing <$ blank)
  where
    blank = count 3 spaceChar
    crate = char '[' *> printChar <* char ']'

data Wharf = Wharf
  { crates :: [[Maybe Crate]],
    labels :: [Int]
  }
  deriving (Eq, Show, Ord)

parseWharf :: CharParser Wharf
parseWharf = Wharf <$> withNewLines parseCrates <*> parseLabels <* newline
  where
    parseCrates :: CharParser [Maybe Crate]
    parseCrates = try parseCrate `sepEndBy` char ' ' -- 'try' for backtracking
    parseLabels :: CharParser [Int]
    parseLabels = char ' ' *> L.decimal `sepEndBy` some (char ' ')

parseInstructions :: CharParser [Instruction]
parseInstructions =
  withNewLines $
    Instruction
      <$> ("move" *> spaceChar *> L.decimal <* spaceChar)
      <*> ("from" *> spaceChar *> L.decimal <* spaceChar)
      <*> ("to" *> spaceChar *> L.decimal)

data Task = Task
  { wharf        :: Wharf,
    instructions :: [Instruction]
  }
  deriving (Eq, Show, Ord)

parser :: CharParser Task
parser = Task <$> parseWharf <* newline <*> parseInstructions

-- parser yoinked from a friend

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i r l = h ++ (r : xs)
  where
    (h, x : xs) = splitAt i l

move :: Difficulty -> Instruction -> [[a]] -> [[a]]
move diff (Instruction amount f t) stacks = (replaceAt from newOrigin . replaceAt to newDest) stacks
  where
    from = f - 1
    to = t - 1
    origin = stacks !! from
    dest = stacks !! to
    (fstS, newOrigin) = splitAt amount origin
    newDest = case diff of
      Easy -> reverse fstS ++ dest
      Hard -> fstS ++ dest

moves :: Difficulty -> [Instruction] -> [[a]] -> [[a]]
moves d i s = foldl' (&) s (map (move d) i)

rowsToStacks :: [[Maybe Crate]] -> [[Crate]]
rowsToStacks = map catMaybes . transpose

solver :: Difficulty -> Task -> String
solver diff (Task (Wharf crates labels) instructions) = concatMap (take 1) (moves diff instructions (rowsToStacks crates))

day5 :: Difficulty -> Problem Task String
day5 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = Just . solver diff,
      printOutput = show
    }
