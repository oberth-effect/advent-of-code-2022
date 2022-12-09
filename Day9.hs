module Day9 where

import           Common
import           Control.Monad
import qualified Control.Monad.State        as St
import           Data.Monoid
import qualified Data.Set                   as S
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pair :: Parser (Char, Int)
pair = (,) <$> printChar <*> (char ' ' *> L.decimal)

parser :: Parser [(Char, Int)]
parser = pair `sepEndBy` eol

type Knot = (Int, Int)

type MoveState = [Knot]

startState i = replicate i (0, 0)

addCoord :: (Int, Int) -> (Int, Int) -> (Int, Int)
addCoord (x, y) (dx, dy) = (x + dx, y + dy)

moveH :: Char -> Knot -> Knot
moveH 'U' = addCoord (0, 1)
moveH 'D' = addCoord (0, -1)
moveH 'L' = addCoord (-1, 0)
moveH 'R' = addCoord (1, 0)
moveH _   = id

moveK :: Knot -> Knot -> Knot
moveK (hx, hy) (tx, ty)
  | (hx - tx) == 0 && abs (hy - ty) >= 2 = (tx, ty + signum (hy - ty))
  | (hy - ty) == 0 && abs (hx - tx) >= 2 = (tx + signum (hx - tx), ty)
  | abs (hx - tx) > 1 && abs (hy - ty) > 0 = (tx + signum (hx - tx), ty + signum (hy - ty))
  | abs (hx - tx) > 0 && abs (hy - ty) > 1 = (tx + signum (hx - tx), ty + signum (hy - ty))
  | otherwise = (tx, ty)

moveTail :: [Knot] -> [Knot]
moveTail [] = []
moveTail [x] = []
moveTail (x : y : xs) = moved : moveTail (moved : xs)
  where
    moved = moveK x y

move :: Char -> St.State MoveState Knot
move ch = do
  kts <- St.get
  let newH = moveH ch (head kts)
  let newT = moveTail (newH : tail kts)
  St.put (newH : newT)
  return (last newT)

moveN :: (Char, Int) -> St.State MoveState [Knot]
moveN (ch, n) = replicateM n (move ch)

executeInstructions :: [(Char, Int)] -> St.State MoveState [[Knot]]
executeInstructions = mapM moveN

runSnake :: Int -> [(Char, Int)] -> Int
runSnake len ins = length $ S.fromList $ concat $ St.evalState (executeInstructions ins) (startState len)

day9 :: Difficulty -> Problem [(Char, Int)] Int
day9 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . runSnake 2
        Hard -> Just . runSnake 10,
      printOutput = show
    }
