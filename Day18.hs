module Day18 where

import           Common
import           Data.Foldable
import qualified Data.Graph                 as G
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Position = (Int, Int, Int)

type Parser = Parsec Void String

pos :: Parser Position
pos = (,,) <$> L.decimal <*> (char ',' *> L.decimal) <*> (char ',' *> L.decimal)

parser :: Parser [Position]
parser = pos `sepEndBy` eol

neigh :: Position -> [Position]
neigh (x, y, z) = [(x, y, z + 1), (x, y, z - 1), (x, y + 1, z), (x, y - 1, z), (x + 1, y, z), (x - 1, y, z)]

surface :: [Position] -> Int
surface vox = sum $ map freeSides vox
  where
    freeSides p = length $ filter (`notElem` vox) (neigh p)

buildAirGraph :: [Position] -> (G.Graph, G.Vertex -> (Position, Position, [Position]), Position -> Maybe G.Vertex)
buildAirGraph vox = G.graphFromEdges [(p, p, airNeigh p) | x <- [0 .. 20], y <- [0 .. 20], z <- [0 .. 20], let p = (x, y, z), p `notElem` vox]
  where
    airNeigh x = filter (`notElem` vox) (neigh x)

innerSurface :: [Position] -> Int
innerSurface vox = surface $ map ((\(a, b, c) -> a) . nfv) (concat (filter (notElem 0) (map toList comp)))
  where
    (gr, nfv, _) = buildAirGraph vox
    comp = G.components gr

day18 :: Difficulty -> Problem [Position] Int
day18 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . surface
        Hard -> Just . \x -> surface x - innerSurface x,
      printOutput = show
    }
