module Days.Day08 (solveA, solveB) where

import Data.Attoparsec.Text (Parser, sepBy, endOfLine, decimal, char)
import Data.List (sortOn, groupBy)
import Data.Text (Text, pack)
import Parse (mustParse)

type Vec3 = (Int, Int, Int)

data Edge = Edge { from :: Int, to :: Int, dist :: Double } deriving (Show)

junctionBox :: Parser Vec3
junctionBox = (,,) <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal

junctionBoxes :: Parser [Vec3]
junctionBoxes = junctionBox `sepBy` endOfLine

distance :: Vec3 -> Vec3 -> Double
distance (x1, y1, z1) (x2, y2, z2) =
  let dx = x2 - x1
      dy = y2 - y1
      dz = z2 - z1
  in sqrt $ fromIntegral ((dx * dx) + (dy * dy) + (dz * dz))

edges :: [Vec3] -> [Edge]
edges pts =
  [Edge { from = i, to = j, dist = (distance pi pj) }
  | (i, pi) <- zip [0..] pts
  , (j, pj) <- zip [0..] pts
  , i < j
  ]

shortestN :: Int -> [Edge] -> [Edge]
shortestN n = take n . sortOn dist

solveA :: Text -> Text
solveA input =
  let ns = mustParse junctionBoxes input 
      es = shortestN 10 $ edges ns
  in pack $ show es

solveB :: Text -> Text
solveB = pack . show . mustParse junctionBoxes
