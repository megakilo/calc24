import Control.Monad (replicateM, replicateM_)
import Data.List (find)
import System.Random (randomRIO)
import Text.Printf

type Number = (Float, String)

main :: IO ()
main =
  replicateM_ 100 $ do
    nums <- randomList 4
    case find (\x -> fst x == 24) $ calc nums of
      Just n -> print $ snd n
      Nothing -> print "No Solution"

randomList :: Int -> IO [Number]
randomList n = do
  nums <- replicateM n $ randomRIO (1, 13 :: Integer)
  print nums
  return (map (\x -> (fromInteger x, show x)) nums)

calc :: [Number] -> [Number]
calc xs
  | length xs == 1 = xs
  | otherwise = reduce xs >>= calc

reduce :: [Number] -> [[Number]]
reduce xs =
  split2 xs 2 >>= (\(taken, nontaken) -> [r : nontaken | r <- combine2 taken])

split2 :: [a] -> Int -> [([a], [a])]
split2 xs 0 = [([], xs)]
split2 xs n
  | length xs <= n = [(xs, [])]
  | otherwise =
    map
      (\(taken, nontaken) -> (taken, (head xs) : nontaken))
      (split2 (tail xs) n) ++
    map
      (\(taken, nontaken) -> ((head xs) : taken, nontaken))
      (split2 (tail xs) (n - 1))

combine2 :: [Number] -> [Number]
combine2 [(x, xstr), (y, ystr)] =
  [ (x + y, printf "(%s + %s)" xstr ystr)
  , (x * y, printf "(%s * %s)" xstr ystr)
  , (x - y, printf "(%s - %s)" xstr ystr)
  , (y - x, printf "(%s - %s)" ystr xstr)
  , (x / y, printf "(%s / %s)" xstr ystr)
  , (y / x, printf "(%s / %s)" ystr xstr)
  ]
combine2 _ = []
