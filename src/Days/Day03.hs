module Days.Day03 (solveA, solveB) where

import Data.Attoparsec.Text
import Data.Char (digitToInt, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Parse (mustParse)

type Joltage = Int

joltages :: Parser [Joltage]
joltages = map digitToInt <$> many1 (satisfy isDigit)

banks :: Parser [[Joltage]]
banks = joltages `sepBy` endOfLine

outputJoltageN :: Int -> [Joltage] -> Joltage
outputJoltageN n js = go 0 n js
  where
    go v 0 _ = v
    go v i xs =
      let maxN = maximum $ Prelude.take (length xs - (i - 1)) xs
          xs' = drop 1 $ dropWhile (< maxN) xs
          v' = v + maxN * (10 ^ (i - 1))
          i' = i - 1
       in go v' i' xs'

solveA :: Text -> Text
solveA = T.show . sum . map (outputJoltageN 2) . mustParse banks

solveB :: Text -> Text
solveB = T.show . sum . map (outputJoltageN 12) . mustParse banks
