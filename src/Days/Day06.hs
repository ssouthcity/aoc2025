module Days.Day06 (solveA, solveB) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, many', many1, sepBy, skipSpace)
import Data.List (transpose)
import Data.Text (Text, pack, unpack)
import Parse (mustParse)

data Op = Add | Mul deriving (Show, Eq)

data Problem = Problem
  { operation :: Op,
    numbers :: [Int]
  }
  deriving (Show, Eq)

parseOp :: Parser Op
parseOp = (Add <$ char '+') <|> (Mul <$ char '*')

literalSpace :: Parser Char
literalSpace = char ' '

parseNums :: Parser [Int]
parseNums = do
  _ <- many' literalSpace
  ns <- decimal `sepBy` (many1 literalSpace)
  _ <- many' literalSpace
  pure ns

parseMatrix :: Parser [[Int]]
parseMatrix = many' (parseNums <* endOfLine)

parseOps :: Parser [Op]
parseOps = parseOp `sepBy` (many' literalSpace)

problems :: Parser [Problem]
problems = do
  ns <- parseMatrix
  ops <- parseOps
  pure $ map (\(a, b) -> Problem a b) (zip ops (transpose ns))

solveProblem :: Problem -> Int
solveProblem p = case operation p of
  Add -> sum (numbers p)
  Mul -> product (numbers p)

rotateR :: Text -> Text
rotateR = pack . unlines . reverse . transpose . lines . unpack

paddedDecimal :: Parser Int
paddedDecimal = skipSpace *> decimal <* skipSpace

parseProblem :: Parser Problem
parseProblem = do
  ns <- paddedDecimal `sepBy` skipSpace
  op <- parseOp
  pure $ Problem op ns

parseProblems :: Parser [Problem]
parseProblems = parseProblem `sepBy` skipSpace

solveA :: Text -> Text
solveA = pack . show . sum . map solveProblem . mustParse problems

solveB :: Text -> Text
solveB = pack . show . sum . map solveProblem . mustParse parseProblems . rotateR
