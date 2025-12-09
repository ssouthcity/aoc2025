{-# LANGUAGE TupleSections #-}

module Days.Day09 (solveA, solveB) where

import Data.Attoparsec.Text (Parser, decimal, char, sepBy, endOfLine)
import Data.List (sort)
import Data.Text (Text, pack)
import Parse (mustParse)

type Vec2 = (Int, Int)
type Rect = (Vec2, Vec2)

vec2 :: Parser Vec2
vec2 = (,) <$> decimal <* char ',' <*> decimal

vec2s :: Parser [Vec2]
vec2s = vec2 `sepBy` endOfLine

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map ((x,)) xs ++ pairs xs

area :: Rect -> Int
area ((x1, y1), (x2, y2)) = w * h
  where 
    w = 1 + abs (x2 - x1)
    h = 1 + abs (y2 - y1)

largestRect :: [Vec2] -> Int
largestRect = last . sort . map area . pairs

solveA :: Text -> Text
solveA = pack . show . largestRect . mustParse vec2s

solveB :: Text -> Text
solveB = pack . show . mustParse vec2s
