module Day10 where

import           Common
import           Control.Monad.State        as S
import           Data.Bifunctor
import           Data.List                  (intercalate)
import           Data.List.Split
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Command = NoOp | AddX Int deriving (Show)

parseCommand :: Parser Command
parseCommand =
  choice
    [ NoOp <$ string "noop",
      AddX <$> (string "addx " *> L.signed space L.decimal)
    ]

doCommand :: Command -> S.State Int [Int]
doCommand NoOp = do
  val <- get
  pure [val]
doCommand (AddX x) = do
  val <- get
  put (val + x)
  pure [val, val + x]

doAll :: [Command] -> S.State Int [Int]
doAll = mapState (first ((:) 1 . concat)) . traverse doCommand

eval :: [Command] -> [Int]
eval cmd = evalState (doAll cmd) 1

sumIndexProd :: [Int] -> [Int] -> Int
sumIndexProd ind arr = sum $ zipWith (*) ind (map ((arr !!) . flip (-) 1) ind)

solve1 :: [Int] -> [Command] -> Int
solve1 ind = sumIndexProd ind . eval

pixels :: [Int]
pixels = [0 .. 239]

solve2 :: [Int] -> [Command] -> String
solve2 px = zipWith pixel px . eval
  where
    pixel index val
      | abs (index `mod` 40 - val) <= 1 = '#'
      | otherwise = '.'

day10 :: Difficulty -> Problem [Command] String
day10 diff =
  Problem
    { parseInput = eitherToMaybe . parse (parseCommand `sepEndBy` eol) "",
      solve = case diff of
        Easy -> Just . show . solve1 [20, 60, 100, 140, 180, 220]
        Hard -> Just . solve2 pixels,
      printOutput = case diff of
        Easy -> show
        Hard -> intercalate "\n" . chunksOf 40
    }
