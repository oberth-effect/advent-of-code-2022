module Day11 where

import           Common
import           Control.Applicative
import           Control.Monad.State as State
import           Data.Bifunctor
import           Data.List           (sortOn)
import           Data.Ord            (Down (Down))

data Monke = Monke
  { items     :: [Int],
    operation :: Int -> Int,
    yeetTest  :: Int,
    yeetTrue  :: Int,
    yeetFalse :: Int
  }

instance Show Monke where
  show m = show (items m)

divTest :: Int -> Int -> Bool
divTest i x = x `mod` i == 0

monkeAppend :: [Int] -> Monke -> Monke
monkeAppend i m = m {items = items m <> i}

monkeClear :: Monke -> Monke
monkeClear m = m {items = []}

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i r l = h <> (r : xs)
  where
    (h, _ : xs) = splitAt i l

yeetPair :: Difficulty -> [Monke] -> Monke -> ([Int], [Int])
yeetPair diff mlist m = (filter (divTest (yeetTest m)) newItems, filter (not . divTest (yeetTest m)) newItems)
  where
    prod = product $ map yeetTest mlist
    newItems = case diff of
      Easy -> map (flip div 3 . flip mod prod . operation m) (items m)
      Hard -> map (flip mod prod . operation m) (items m)

yeet :: Difficulty -> Int -> State.State [Monke] Int
yeet diff ind = do
  monkeList <- State.get
  let monke = monkeList !! ind
  let (yeetT, yeetF) = yeetPair diff monkeList monke
  let newTmonke = monkeAppend yeetT (monkeList !! yeetTrue monke)
  let newFmonke = monkeAppend yeetF (monkeList !! yeetFalse monke)
  let newMonke = monkeClear monke
  State.put ((replaceAt ind newMonke . replaceAt (yeetTrue monke) newTmonke . replaceAt (yeetFalse monke) newFmonke) monkeList)
  pure (length (items monke))

yeetRound :: Difficulty -> State.State [Monke] (ZipList Int)
yeetRound diff = do
  monkeList <- State.get
  traverse (yeet diff) (ZipList [0 .. length monkeList - 1])

yeetRounds :: Int -> Difficulty -> State.State [Monke] (ZipList Int)
yeetRounds x diff = State.mapState (first $ fmap sum . sequenceA) (replicateM x (yeetRound diff))

evalRounds :: Int -> Difficulty -> [Monke] -> [Int]
evalRounds x diff m = getZipList (evalState (yeetRounds x diff) m)


-- Too Lazy to write a parser today
exMonkes =
  [ Monke [79, 98] (* 19) 23 2 3,
    Monke [54, 65, 75, 74] (+ 6) 19 2 0,
    Monke [79, 60, 97] (\x -> x * x) 13 1 3,
    Monke [74] (+ 3) 17 0 1
  ]

inMonkes =
  [ Monke [98, 89, 52] (* 2) 5 6 1,
    Monke [57, 95, 80, 92, 57, 78] (* 13) 2 2 6,
    Monke [82, 74, 97, 75, 51, 92, 83] (+ 5) 19 7 5,
    Monke [97, 88, 51, 68, 76] (+ 6) 7 0 4,
    Monke [63] (+ 1) 17 0 1,
    Monke [94, 91, 51, 63] (+ 4) 13 4 3,
    Monke [61, 54, 94, 71, 74, 68, 98, 83] (+ 2) 3 2 7,
    Monke [90, 56] (\x -> x * x) 11 3 5
  ]

day11 :: Difficulty -> Problem [Monke] Int
day11 diff =
  Problem
    { parseInput = Just . const inMonkes,
      solve = case diff of
        Easy -> Just . product . take 2 . sortOn Down . evalRounds 20 Easy
        Hard -> Just . product . take 2 . sortOn Down . evalRounds 10000 Hard,
      printOutput = show
    }
