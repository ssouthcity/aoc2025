{-# LANGUAGE TypeApplications #-}

module Days.Day01 (solveA, solveB) where

import Data.Text (Text, pack, unpack)

type Pos = Int
data Dir = L | R deriving (Show)
type Rot = (Dir, Int)

dialStart :: Pos
dialStart = 50

dialCount :: Pos
dialCount = 100

parseRot :: String -> Rot
parseRot (x:xs) = (if x == 'L' then L else R, read xs)
parseRot [] = (L, 0)

applyRot :: Pos -> Rot -> Pos
applyRot x (L, y) = x - y
applyRot x (R, y) = x + y

normalizeDial :: Pos -> Pos
normalizeDial x = x `mod` dialCount

countSweeps :: Pos -> Rot -> Int
countSweeps pos (dir, n) = 
    let first = case dir of
            L -> normalizeDial pos
            R -> normalizeDial (dialCount - pos)
    in 
      if first == 0 && n > dialCount
      then (n - first) `div` dialCount
      else if n > first
      then 1 + (n - first) `div` dialCount
      else 0

countZeroes :: Pos -> [Rot] -> Int
countZeroes = go 0
  where
    go acc _ [] = acc
    go acc start (rot:rs) =
      let end = normalizeDial (applyRot start rot)
          sweeps = countSweeps start rot
      in go (acc + sweeps) end rs

solveA :: Text -> Text
solveA = pack . show . length . filter (== 0) . map (normalizeDial) . scanl applyRot dialStart . map parseRot . lines . unpack

solveB :: Text -> Text
solveB = pack . show . countZeroes dialStart . map parseRot . lines . unpack
