module Day13 where

import           Common
import           Data.List                  (elemIndex, sort)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Item = Value Int | List [Item] deriving (Eq, Show)

item :: Parser Item
item =
  choice
    [ Value <$> L.decimal,
      List <$> (char '[' *> (item `sepBy` char ',') <* char ']')
    ]

pair :: Parser (Item, Item)
pair = (,) <$> item <*> (eol *> item)

parser :: Parser [(Item, Item)]
parser = pair `sepBy` try (eol *> eol)

instance Ord Item where
  compare (Value a) (Value b) = compare a b
  compare (List []) (List y) = LT
  compare (List x) (List []) = GT
  compare (List (x : xs)) (List (y : ys))
    | x == y = compare (List xs) (List ys)
    | otherwise = compare x y
  compare (Value a) (List y) = compare (List [Value a]) (List y)
  compare (List x) (Value a) = compare (List x) (List [Value a])

ind :: Int -> Bool -> Int
ind x True  = x
ind _ False = 0

pairsToList :: [(a, a)] -> [a]
pairsToList = concatMap (\(a, b) -> [a, b])

solve2 :: [(Item, Item)] -> Maybe Int
solve2 pairs = (*) <$> add 1 (elemIndex div1 arr) <*> add 1 (elemIndex div2 arr)
  where
    add a b = (+) <$> Just a <*> b
    div1 = List [List [Value 2]]
    div2 = List [List [Value 6]]
    arr = sort ([div1, div2] <> pairsToList pairs)

day13 :: Difficulty -> Problem [(Item, Item)] Int
day13 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . sum . zipWith ind [1 ..] . map (uncurry (<))
        Hard -> solve2,
      printOutput = show
    }
