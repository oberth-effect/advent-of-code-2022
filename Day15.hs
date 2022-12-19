module Day15 where

import           Common
import           Data.List
import qualified Data.Map.Strict            as M
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Position = (Int, Int)

type Interval = (Int, Int)

type Lines = M.Map Int [Interval]

pos :: Parser Position
pos = (,) <$> (string "x=" *> L.signed space L.decimal) <*> (string ", y=" *> L.signed space L.decimal)

parsePositions :: Parser (Position, Position)
parsePositions = (,) <$> (string "Sensor at " *> pos) <*> (string ": closest beacon is at " *> pos)

parser :: Parser [(Position, Position)]
parser = parsePositions `sepEndBy` eol

reduceOverlaps :: [Interval] -> [Interval]
reduceOverlaps [] = []
reduceOverlaps [x] = [x]
reduceOverlaps ((a, b) : (c, d) : xs)
      | c <= b = reduceOverlaps ((a, max b d) : xs)
      | otherwise = (a, b) : reduceOverlaps ((c, d) : xs)

scanLine :: Int -> [(Position, Position)] -> Int
scanLine ln bcns =  sum (map (\(a,b) -> b-a+1) $ M.findWithDefault [] ln (buildMap ln ln bcns)) - inlineBeacons
  where inlineBeacons = length (nub (filter (\(_,b) -> b == ln) (map snd bcns)))

scanSector :: Int -> Int -> [(Position,Position)] -> Int
scanSector lw up bcns = freq $ head $ M.assocs (M.filter ((>1).length)(buildMap lw up bcns))


buildMap :: Int-> Int ->  [(Position, Position)] -> Lines
buildMap lw up bcns =  M.map reduceOverlaps (M.map (sortBy (\(a, b) (c, d) -> compare a c)) (M.fromList [(y, inters y)| y <- [lw .. up]]))
  where inters ln = filter (uncurry (<=)) [(fst s - rng + diff, fst s + rng - diff)|(s,b) <- bcns, let diff = abs(snd s - ln), let rng = dist s b]

dist :: Position -> Position -> Int
dist (x, y) (sx, sy) = abs (x - sx) + abs (y - sy)

dist' :: (Position,Position) -> Int
dist' ((x,y),(sx,sy)) =  abs (x - sx) + abs (y - sy)

freq :: (Int, [Interval])-> Int
freq (y, xs) = x * 4000000 + y
  where x = snd (head xs) + 1

day15 :: Difficulty -> Problem [(Position, Position)] Int
day15 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . scanLine 2000000
        Hard -> Just . scanSector 0 4000000,
      printOutput = show
    }
