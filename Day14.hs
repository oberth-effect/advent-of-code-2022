module Day14 where

import           Common
import qualified Data.Set                   as S
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Position = (Int, Int)

type Bounds = ((Int, Int), (Int, Int))

type Pth = [Position]

type Maze = (S.Set Position, Bounds)

type Parser = Parsec Void String

pos :: Parser Position
pos = (,) <$> L.decimal <*> (char ',' *> L.decimal)

pth :: Parser Pth
pth = pos `sepEndBy` string " -> "

parser :: Parser [Pth]
parser = pth `sepEndBy` eol

sandStart :: Position
sandStart = (500, 0)

bounds :: [Pth] -> ((Int, Int), (Int, Int))
bounds parr = ((minimum horizontal, maximum horizontal), (minimum vertical, maximum vertical))
  where
    horizontal = [fst sandStart] <> map fst (concat parr)
    vertical = [snd sandStart] <> map snd (concat parr)

pthToCoord :: Pth -> [Position]
pthToCoord = foldl line []
  where
    range a b
      | a < b = [a .. b]
      | otherwise = reverse [b .. a]
    line :: [Position] -> Position -> [Position]
    line [] pos = [pos]
    line ((px, py) : ps) (x, y) = reverse [(x, y) | x <- range px x, y <- range py y] <> ps

buildMaze :: [Pth] -> Maze
buildMaze pths = (foldl (flip S.insert) S.empty (concatMap pthToCoord pths), bounds pths)

validMoves :: Position -> S.Set Position -> [Position]
validMoves (x, y) blocks = filter (not . flip S.member blocks) [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

outOfBounds :: Position -> Bounds -> Bool
outOfBounds (x, y) (xb, yb) = out x xb || out y yb
  where
    out p (a, b) = p < a || p > b

sendSand :: Position -> Maze -> (Bool, Maze)
sendSand pos (blocks, bnds)
  | outOfBounds pos bnds = (True, (blocks, bnds))
  | not (null (validMoves pos blocks)) = sendSand (head (validMoves pos blocks)) (blocks, bnds)
  | otherwise = (False, (S.insert pos blocks, bnds))

sendSand2 :: Position -> Maze -> (Bool, Maze)
sendSand2 pos (blocks, bnds)
  | snd pos == snd (snd bnds) + 1 = (False, (S.insert pos blocks, bnds))
  | not (null (validMoves pos blocks)) = sendSand2 (head (validMoves pos blocks)) (blocks, bnds)
  | null (validMoves pos blocks) && pos == sandStart = (True, (blocks, bnds))
  | otherwise = (False, (S.insert pos blocks, bnds))

runSand :: Difficulty -> (Int, (Bool, Maze)) -> Int
runSand diff (runs, (fin, maze))
  | fin = runs - offset
  | otherwise = runSand diff (runs + 1, send sandStart maze)
  where
    offset = case diff of
      Easy -> 1
      Hard -> 0
    send = case diff of
      Easy -> sendSand
      Hard -> sendSand2

day14 :: Difficulty -> Problem [Pth] Int
day14 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = Just . \x -> runSand diff (0, (False, buildMaze x)),
      printOutput = show
    }
