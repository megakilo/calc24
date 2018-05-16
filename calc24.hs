import Control.Monad (replicateM, replicateM_)
import Data.List (find)
import System.Random (randomRIO)
import Text.Printf (printf)

type Number = (Float, String)

main :: IO ()
main =
  replicateM_ 1000 $ do
    nums <- randomList 4
    putStr $ show (map (round . fst) nums) ++ " -> "
    case find (\x -> fst x == 24) $ calc nums of
      Just n -> putStrLn $ snd n
      Nothing -> putStrLn "No Solution"

randomList :: Int -> IO [Number]
randomList n = do
  nums <- replicateM n $ randomRIO (1, 13 :: Integer)
  return $ map createNumber nums
  where
    createNumber x = (fromInteger x, show x)

calc :: [Number] -> [Number]
calc xs
  | length xs == 1 = xs
  | otherwise = reduce xs >>= calc
  where
    reduce xs =
      split xs 2 >>=
      (\([num1, num2], nontaken) -> [r : nontaken | r <- combine num1 num2])

split :: [a] -> Int -> [([a], [a])]
split xs 0 = [([], xs)]
split xs n
  | length xs <= n = [(xs, [])]
  | otherwise =
    map (\pair -> (fst pair, head xs : snd pair)) (split (tail xs) n) ++
    map (\pair -> (head xs : fst pair, snd pair)) (split (tail xs) (n - 1))

combine :: Number -> Number -> [Number]
combine (x, xstr) (y, ystr) =
  [ (x + y, printf "(%s + %s)" xstr ystr)
  , (x * y, printf "(%s * %s)" xstr ystr)
  , (x - y, printf "(%s - %s)" xstr ystr)
  , (y - x, printf "(%s - %s)" ystr xstr)
  , (x / y, printf "(%s / %s)" xstr ystr)
  , (y / x, printf "(%s / %s)" ystr xstr)
  ]