module Day8 where

import Common
import Control.Arrow (Arrow (arr))
import Data.Char
import Data.List
import Data.List.HT
import Data.List.Split

splitT :: [a] -> Int -> ([a], a, [a])
splitT arr i = (pre, x, suff)
  where
    (f, suff) = splitAt i arr
    pre = init f
    x = last f

visible1 :: ([Int], Int, [Int]) -> Bool
visible1 ([], _, _) = True
visible1 (_, _, []) = True
visible1 (pre, x, suff) = all (< x) pre || all (< x) suff

visible2 :: ([Int], Int, [Int]) -> Int
visible2 ([], _, _) = 0
visible2 (_, _, []) = 0
visible2 (pre, x, suff) = length (takeUntil (>= x) (reverse pre)) * length (takeUntil (>= x) suff)

solve1 :: [[Int]] -> [[Bool]]
solve1 arr = zipWith (zipWith (||)) (visibilityR arr) (transpose (visibilityR (transpose arr)))
  where
    visibilityR a = [visibility t | t <- a]
    visibility l = map (visible1 . splitT l) [1 .. length l]

solve2 :: [[Int]] -> [[Int]]
solve2 arr = zipWith (zipWith (*)) (visibilityR arr) (transpose (visibilityR (transpose arr)))
  where
    visibilityR a = [visibility t | t <- a]
    visibility l = map (visible2 . splitT l) [1 .. length l]

day8 :: Difficulty -> Problem [[Int]] Int
day8 diff =
  Problem
    { parseInput = Just . map (map digitToInt) . lines,
      solve = case diff of
        Easy -> Just . length . filter (== True) . concat . solve1
        Hard -> Just . maximum . concat . solve2,
      printOutput = show
    }
