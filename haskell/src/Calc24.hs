module Calc24 (calc24) where

import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Text.Printf (printf)

data Number = Number
  { value :: Float,
    expression :: String,
    operator :: Char
  }

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
    map (\(taken, nontaken) -> (taken, h : nontaken)) (split t n)
      ++ map (\(taken, nontaken) -> (h : taken, nontaken)) (split t (n - 1))

combine :: Number -> Number -> [Number]
combine (Number x xExpr xOp) (Number y yExpr yOp) =
  [ Number (x + y) (printf "%s + %s" xExpr yExpr) '+',
    Number (x * y) (printf "%s * %s" (addParentheses xExpr xOp False) (addParentheses yExpr yOp False)) '*',
    Number (x / y) (printf "%s / %s" (addParentheses xExpr xOp False) (addParentheses yExpr yOp True)) '/',
    Number (y / x) (printf "%s / %s" (addParentheses yExpr yOp False) (addParentheses xExpr xOp True)) '/',
    if x > y
      then Number (x - y) (printf "%s - %s" xExpr (addParentheses yExpr yOp False)) '-'
      else Number (y - x) (printf "%s - %s" yExpr (addParentheses xExpr xOp False)) '-'
  ]
  where
    addParentheses expr op isDenominator
      | op `elem` "+-" || isDenominator = "(" ++ expr ++ ")"
      | otherwise = expr
