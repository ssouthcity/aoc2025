{-# LANGUAGE TypeApplications #-}

module Days.Day02 (solveA, solveB) where

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Ix as Ix
import Parse (mustParse)

type Range = (Int, Int)

range :: Parser Range
range = (,) <$> decimal <* char '-' <*> decimal

ranges :: Parser [Range]
ranges = range `sepBy` char ','

isRepeatedTwice :: String -> Bool
isRepeatedTwice s
  | odd (length s) = False
  | otherwise = let (a, b) = splitAt (length s `div` 2) s in a == b

chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n s  = Prelude.take n s : chunksOf n (drop n s)

permutations :: String -> [[String]]
permutations s =
  let len = length s
      divisors = [d | d <- [1..len `div` 2], len `mod` d == 0]
  in [chunksOf d s | d <- divisors]

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

isRepeatedN :: String -> Bool
isRepeatedN = any allEqual . permutations

solveA :: Text -> Text
solveA = T.show . sum . map (read @Int) . concat . map (filter isRepeatedTwice . map show . Ix.range) . mustParse ranges

solveB :: Text -> Text
solveB = T.show . sum . map (read @Int) . concat . map (filter isRepeatedN . map show . Ix.range) . mustParse ranges
