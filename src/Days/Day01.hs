module Days.Day01 (solveA, solveB) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy)
import Data.Text (Text, pack)
import Parse (mustParse)

data Dir = L | R deriving (Show)

type Rot = (Dir, Int)

dir :: Parser Dir
dir = L <$ char 'L' <|> R <$ char 'R'

rot :: Parser Rot
rot = (,) <$> dir <*> decimal

rots :: Parser [Rot]
rots = rot `sepBy` endOfLine

applyRot :: Int -> Rot -> Int
applyRot x (L, y) = x - y
applyRot x (R, y) = x + y

countSweeps :: Int -> Int -> Rot -> Int
countSweeps count x (dir, y) =
  let delta = y `mod` count
      extra = case dir of
        L | x > 0 && x <= delta -> 1
        R | x > 0 && x >= count - delta -> 1
        _ -> 0
   in extra + y `div` count

zeroTicks :: Int -> Int -> [Rot] -> (Int, Int)
zeroTicks start count rs = foldl go (start, 0) rs
  where
    go (p, s) r =
      let p' = (applyRot p r) `mod` count
          s' = s + countSweeps count p r
       in (p', s')

solveA :: Text -> Text
solveA = pack . show . length . filter (== 0) . map (flip mod 100) . scanl applyRot 50 . mustParse rots

solveB :: Text -> Text
solveB = pack . show . snd . zeroTicks 50 100 . mustParse rots
