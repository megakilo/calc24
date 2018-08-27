import Control.Monad (replicateM, replicateM_)
import Data.List (find)
import System.Random (randomRIO)
import Text.Printf (printf)

data Number = Number
  { value :: Float
  , expression :: String
  , operator :: Char
  }

main :: IO ()
main =
  replicateM_ 1000 $ do
    nums <- randomList 4
    let challenge = show $ map (round . value) nums
    case find (\num -> value num == 24) $ calc nums of
      Just num -> putStrLn (challenge ++ " -> " ++ expression num)
      Nothing -> putStrLn (challenge ++ " -> No Solution")

randomList :: Int -> IO [Number]
randomList n = do
  nums <- replicateM n $ randomRIO (1, 13 :: Integer)
  return $ map (\x -> Number (fromInteger x) (show x) 'X') nums

calc :: [Number] -> [Number]
calc xs
  | length xs == 1 = xs
  | otherwise = split xs 2 >>= reduce >>= calc
  where
    reduce ([num1, num2], nontaken) = [r : nontaken | r <- combine num1 num2]

split :: [a] -> Int -> [([a], [a])]
split xs 0 = [([], xs)]
split xs n
  | length xs <= n = [(xs, [])]
  | otherwise =
    map (\pair -> (fst pair, head xs : snd pair)) (split (tail xs) n) ++
    map (\pair -> (head xs : fst pair, snd pair)) (split (tail xs) (n - 1))

combine :: Number -> Number -> [Number]
combine (Number x xExpr xOp) (Number y yExpr yOp) =
  [ Number (x + y) (printf "%s + %s" xExpr yExpr) '+'
  , Number (x * y) (printf "%s * %s" (addParentheses xExpr xOp "+-") (addParentheses yExpr yOp "+-")) '*'
  , Number (x / y) (printf "%s / %s" (addParentheses xExpr xOp "+-") (addParentheses yExpr yOp "+-*/")) '/'
  , Number (y / x) (printf "%s / %s" (addParentheses yExpr yOp "+-") (addParentheses xExpr xOp "+-*/")) '/'
  , if x > y
      then Number (x - y) (printf "%s - %s" xExpr (addParentheses yExpr yOp "+-")) '-'
      else Number (y - x) (printf "%s - %s" yExpr (addParentheses xExpr xOp "+-")) '-'
  ]
  where
    addParentheses expr op conditions
      | op `elem` conditions = "(" ++ expr ++ ")"
      | otherwise = expr
