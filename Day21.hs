module Day21 where

import           Common
import           Control.Monad
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Void
import           Numeric                    (showFFloat)
import qualified Numeric
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Op = Add | Sub | Div | Mult deriving (Eq, Show)

data Expr = Val Integer | Eqn (String, Op, String) deriving (Eq, Show)

type Monke = (String, Expr)

type MonkeMap = M.Map String Expr

parseOp :: Parser Op
parseOp =
  choice
    [ Add <$ char '+',
      Sub <$ char '-',
      Div <$ char '/',
      Mult <$ char '*'
    ]

parseExpression :: Parser Expr
parseExpression =
  Val <$> try L.decimal
    <|> Eqn <$> ((,,) <$> (many letterChar <* space) <*> parseOp <*> (space *> many letterChar))

parseMonke :: Parser Monke
parseMonke = (,) <$> (many letterChar <* string ": ") <*> parseExpression

parser :: Parser MonkeMap
parser = M.fromList <$> parseMonke `sepEndBy` eol

-- assume only linear division
polyDiv :: Poly -> Poly -> Poly
polyDiv (Poly x) (Poly y) = Poly (M.map (\a -> a / (y M.! 0)) x)

evaluate :: String -> Maybe String -> MonkeMap -> Maybe Poly
evaluate key unk monkes = case unk of
  Nothing -> eval key
  Just x
    | x == key -> Just $ Poly (M.fromList [(1, 1), (0, 0)])
    | otherwise -> eval key
  where
    eval k = case M.lookup k monkes of
      Nothing -> Nothing
      Just e -> case e of
        Val x -> Just (fromInteger x)
        Eqn (m, op, n) -> case op of
          Add  -> (+) <$> evaluate m unk monkes <*> evaluate n unk monkes
          Sub  -> (-) <$> evaluate m unk monkes <*> evaluate n unk monkes
          Div  -> polyDiv <$> evaluate m unk monkes <*> evaluate n unk monkes
          Mult -> (*) <$> evaluate m unk monkes <*> evaluate n unk monkes

newtype Poly = Poly (M.Map Int Double) deriving (Eq, Show)

instance Num Poly where
  (+) (Poly x) (Poly y) = Poly $ M.unionWith (+) x y
  negate (Poly x) = Poly $ M.map negate x
  (*) (Poly x) (Poly y) = Poly $ M.fromList [(d + e, a * b) | (d, a) <- M.assocs x, (e, b) <- M.assocs y]
  fromInteger x = Poly $ M.singleton 0 (fromIntegral x)
  abs (Poly x) = Poly $ M.map abs x
  signum _ = 0

-- assume only linear polynoms
solvePoly :: Poly -> Maybe Int
solvePoly (Poly x)
  | maximum (M.keys x) == 0 = Just $ round (x M.! 0)
  | maximum (M.keys x) == 1 = Just $ round (negate (x M.! 0) / (x M.! 1))
  | otherwise = Nothing

day21 :: Difficulty -> Problem MonkeMap Int
day21 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> solvePoly <=< evaluate "root" Nothing
        Hard -> \x -> solvePoly =<< evaluate "root" (Just "humn") (M.update (\(Eqn (a, _, b)) -> Just (Eqn (a, Sub, b))) "root" x),
      printOutput = show
    }
