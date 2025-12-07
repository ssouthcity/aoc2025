module Days.Day05 (solveA, solveB) where

import Data.Attoparsec.Text
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Parse (mustParse)

type Range = (Int, Int)

range :: Parser Range
range = (,) <$> decimal <* char '-' <*> decimal

ranges :: Parser [Range]
ranges = range `sepBy` endOfLine

ingredients :: Parser [Int]
ingredients = decimal `sepBy` endOfLine

input :: Parser ([Range], [Int])
input = (,) <$> ranges <* skipMany endOfLine <*> ingredients

inRange :: Range -> Int -> Bool
inRange (lower, upper) n = n >= lower && n <= upper

inAnyRange :: [Range] -> Int -> Bool
inAnyRange rs n = any ($ n) (map inRange rs)

sizeOfRange :: Range -> Int
sizeOfRange (lower, upper) = upper - lower + 1

mergeRanges :: [Range] -> [Range]
mergeRanges = reverse . foldl go [] . sortOn fst
  where
    go [] r = [r]
    go acc@((l2, u2) : rs) (l1, u1)
      | u2 >= l1 - 1 = (l2, max u2 u1) : rs
      | otherwise = (l1, u1) : acc

solveA :: Text -> Text
solveA i =
  let (rs, is) = mustParse input i
      fresh = filter (inAnyRange rs) is
   in T.show $ length fresh

solveB :: Text -> Text
solveB = T.show . sum . map sizeOfRange . mergeRanges . mustParse ranges
