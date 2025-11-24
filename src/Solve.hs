module Solve (standardSolve) where

import Data.Text (Text, pack)
import qualified Data.Text.IO as T

standardSolve :: (Text -> Text) -> (Text -> Text) -> IO ()
standardSolve solveA solveB = do
  input <- T.getContents
  T.putStrLn $ pack "[Part A]"
  T.putStrLn $ solveA input
  T.putStrLn $ pack "[Part B]"
  T.putStrLn $ solveB input
