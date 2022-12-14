{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import           Common
import           Data.Bifoldable
import           Data.Function
import qualified Data.Map                   as M
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List

data Command
  = CdRoot
  | CdUp
  | CdDir String
  | Ls [LsOutput]
  deriving (Eq, Show, Ord)

data LsOutput
  = LsFile Int String
  | LsDir String
  deriving (Eq, Show, Ord)

type CharParser a = Parsec Void String a

withNewLines :: CharParser a -> CharParser [a]
withNewLines = flip sepEndBy newline

parseFilename :: CharParser String
parseFilename = some (printChar <|> char '.')

parseLsOutput :: CharParser LsOutput
parseLsOutput = parseLsDir <|> parseLsFile
  where
    parseLsDir = LsDir <$> ("dir" *> char ' ' *> some printChar)
    parseLsFile = LsFile <$> (L.decimal <* char ' ') <*> parseFilename

parseCommand :: CharParser Command
parseCommand =
  "$ "
    *> choice
      [ CdRoot <$ "cd /" <* newline,
        CdUp <$ "cd .." <* newline,
        CdDir <$> ("cd " *> some printChar <* newline),
        Ls <$> ("ls" *> newline *> withNewLines parseLsOutput)
      ]

type Path = [String]

type Fs = M.Map Path Int

movePath :: Command -> Path -> Path
movePath CdRoot _      = ["/"]
movePath CdUp (p : ps) = ps
movePath (CdDir d) p   = d : p
movePath _ p           = p

localSize :: [LsOutput] -> Int
localSize = foldl' countFiles 0
  where
    countFiles :: Int -> LsOutput -> Int
    countFiles buff (LsFile size _) = buff + size
    countFiles buff _ = buff

resolveCommand :: (Path, Fs) -> Command -> (Path, Fs)
resolveCommand (pth, fs) (Ls lscont) = (pth, M.insert pth (localSize lscont) fs)
resolveCommand (pth, fs) cmd = (movePath cmd pth, fs)

runCommands :: [Command] -> (Path, Fs)
runCommands = foldl resolveCommand ([], M.empty)

folderSize :: Fs -> Path -> Int
folderSize fs pth = fs M.! pth + subfolderSizes
  where
    subfolderSizes = sum [fs M.! p | p <- M.keys fs, pth `elem` tails p, p/=pth]

sizes :: Fs -> [Int]
sizes fs = map (folderSize fs) (M.keys fs)

freeSpace :: Fs -> Int
freeSpace fs = 70000000 - folderSize fs ["/"]

wouldFree :: Fs -> Int -> Bool
wouldFree fs size = freeSpace fs + size >= 30000000

solve2 :: Fs -> Int
solve2 fs = (head . filter (wouldFree fs) . sort . sizes) fs

day7 :: Difficulty -> Problem [Command] Int
day7 diff =
  Problem
    { parseInput = eitherToMaybe . parse (many parseCommand) "",
      solve = case diff of
        Easy -> Just . sum . filter (<= 100000) . sizes . snd . runCommands
        Hard -> Just . solve2 . snd . runCommands,
      printOutput = show
    }
