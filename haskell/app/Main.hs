module Main (main) where

import Calc24 (calc24)
import Control.Monad (replicateM, replicateM_)
import System.Random (randomRIO)

main :: IO ()
main = replicateM_ 1000 $ do
  nums <- replicateM 4 $ randomRIO (1, 13 :: Integer)
  putStrLn $ case calc24 nums of
    Just result -> show nums ++ " -> " ++ result
    Nothing -> show nums ++ " -> No Solution"