module Calc24 (main, calc24) where

import Control.Monad (replicateM, replicateM_)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import System.Random (randomRIO)
import Text.Printf (printf)

data Number = Number
  { value :: Float,
    expression :: String,
    operator :: Char
  }

main :: IO ()
main = replicateM_ 1000 $ do
  nums <- replicateM 4 $ randomRIO (1, 13 :: Integer)
  putStrLn $ case calc24 nums of
    Just result -> show nums ++ " -> " ++ result
    Nothing -> show nums ++ " -> No Solution"

calc24 :: [Integer] -> Maybe String
calc24 nums =
  let numbers = map (\x -> Number (fromInteger x) (show x) 'X') nums
   in calc numbers 24

calc :: [Number] -> Float -> Maybe String
calc [] _ = Nothing
calc [x] target = if value x == target then Just (expression x) else Nothing
calc nums target = fromMaybe Nothing (find isJust [calc xs target | xs <- reducedList])
  where
    reducedList = split nums 2 >>= \([num1, num2], nontaken) -> [r : nontaken | r <- combine num1 num2]

split :: [a] -> Int -> [([a], [a])]
split [] _ = []
split xs 0 = [([], xs)]
split xs@(h : t) n
  | length xs <= n = [(xs, [])]
  | otherwise =
    map (\pair -> (fst pair, h : snd pair)) (split t n)
      ++ map (\pair -> (h : fst pair, snd pair)) (split t (n - 1))

combine :: Number -> Number -> [Number]
combine (Number x xExpr xOp) (Number y yExpr yOp) =
  [ Number (x + y) (printf "%s + %s" xExpr yExpr) '+',
    Number (x * y) (printf "%s * %s" (addParentheses xExpr xOp "+-") (addParentheses yExpr yOp "+-")) '*',
    Number (x / y) (printf "%s / %s" (addParentheses xExpr xOp "+-") (addParentheses yExpr yOp "+-*/")) '/',
    Number (y / x) (printf "%s / %s" (addParentheses yExpr yOp "+-") (addParentheses xExpr xOp "+-*/")) '/',
    if x > y
      then Number (x - y) (printf "%s - %s" xExpr (addParentheses yExpr yOp "+-")) '-'
      else Number (y - x) (printf "%s - %s" yExpr (addParentheses xExpr xOp "+-")) '-'
  ]
  where
    addParentheses expr op conditions
      | op `elem` conditions = "(" ++ expr ++ ")"
      | otherwise = expr
