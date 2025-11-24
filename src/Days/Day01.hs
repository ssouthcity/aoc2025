{-# LANGUAGE TypeApplications #-}

module Days.Day01 (solveA, solveB) where

import Data.Text (Text, pack, unpack)

solveA :: Text -> Text
solveA = pack . show . sum . map (read @Int) . lines . unpack

solveB :: Text -> Text
solveB input = input
