module Day12 where

import Common
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Tree
import Data.List
import Data.Maybe

type Position = Int
type Terrain = [String]

conn :: Position -> Terrain -> [Edge]
conn p terr = map (\x -> (p,x)) (filter reachable neighbours)
  where
    rows = length terr
    cols = length $ head terr
    neighbours = filter (\x -> x > 0 && x < rows * cols) [p-1,p+1, p-cols, p+ cols]
    getHeight x = concat terr !! x
    reachable x
      | getHeight p == 'S' = getHeight x <= 'b'
      | getHeight x == 'E' = getHeight p >= 'y'
      | getHeight x <= succ (getHeight p) = True
      | otherwise = False


makeGraph ::  Terrain -> Gr () ()
makeGraph terr = mkUGraph [0 .. length (concat terr) - 1] (concat [conn x terr | x<- [0..length (concat terr) -1]])

solve1 :: Terrain -> Path
solve1 terr = esp (fromMaybe 0 (elemIndex 'S' (concat terr))) (fromMaybe 0 (elemIndex 'E' (concat terr))) (makeGraph terr :: Gr () ())


solve2 terr = minimum $ filter (/=0) $ map (\x -> length (esp x end graph)) (elemIndices 'a' (concat terr))
  where
    end = fromMaybe 0 (elemIndex 'E' (concat terr))
    graph = makeGraph terr

day12 :: Difficulty -> Problem [String] Int
day12 diff =
  Problem
    { parseInput = Just . lines,
      solve = case diff of
        Easy -> Just . flip (-) 1 . length . solve1
        Hard -> Just . flip (-) 1 . solve2,
      printOutput = show
    }
