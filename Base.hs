module Base where
import Basement.Imports (Show)

data Difficulty = Easy | Hard

data Problem a b = Problem
  { parseInput :: String -> Maybe a,
    solve :: Difficulty -> a -> b
  }

aocSolve :: (Show a, Show b) => (Problem a b) -> String -> Maybe (b, b)
aocSolve p s = case maybedata of
  Just d -> Just (solve p Easy d, solve p Hard d)
  Nothing -> Nothing
  where
    maybedata = parseInput p s

aocRun :: (Show a, Show b) => Problem a b -> IO ()
aocRun p = do
    c <- getContents
    case aocSolve p c of
      Nothing -> do
        putStrLn "Error parsing input"
      Just r -> do
        print $ fst r
        print $ snd r
