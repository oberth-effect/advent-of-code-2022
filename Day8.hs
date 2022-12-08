module Day8 where

import           Common
import           Control.Arrow   (Arrow (arr))
import           Data.Char
import           Data.List
import           Data.List.HT

visible1 :: ([Int], Int, [Int]) -> Bool
visible1 (pre, x, suff) = all (< x) pre || all (< x) suff

visible2 :: ([Int], Int, [Int]) -> Int
visible2 (pre, x, suff) = length (takeUntil (>= x) (reverse pre)) * length (takeUntil (>= x) suff)

zipMatrixWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrixWith f = zipWith (zipWith f)

applyZipFun :: ([[a]] -> [[b]]) -> (b -> b -> c) -> [[a]] -> [[c]]
applyZipFun fun zipper arr = zipMatrixWith zipper (fun arr) (trf fun arr)
  where
    trf f = transpose . f . transpose

applyOnInner :: (([a], a, [a]) -> b) -> [[a]] -> [[b]]
applyOnInner fun arr = [[fun (triplet i row) | i <- [0 .. length row - 1]] | row <- arr]
  where
    triplet i arr = (take i arr, arr !! i, drop (i + 1) arr)

day8 :: Difficulty -> Problem [[Int]] Int
day8 diff =
  Problem
    { parseInput = Just . map (map digitToInt) . lines,
      solve = case diff of
        Easy -> Just . length . filter (== True) . concat . applyZipFun (applyOnInner visible1) (||)
        Hard -> Just . maximum . concat . applyZipFun (applyOnInner visible2) (*),
      printOutput = show
    }
