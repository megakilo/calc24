import Data.List
import Data.Bool
import Text.Printf
import Control.Monad (replicateM)
import System.Random (randomRIO)

data Number = Number { getValue :: Float, getExpr :: String } deriving (Show, Eq) 

main :: IO ()
main = do
    nums <- randomList 4
    case find (\x -> getValue x == 24) $ calc nums of
        Just n -> print $ getExpr n
        Nothing -> print "No Solution"

randomList :: Int -> IO [Number]
randomList n = do 
    nums <- replicateM n $ randomRIO (1,13::Integer)
    --let nums = [1,2,3,4]
    print nums
    return (map (\x -> Number (fromInteger x) (show x)) nums)

calc :: [Number] -> [Number]
calc xs
    | length xs == 1 = xs
    | otherwise = reduce xs >>= calc

reduce :: [Number] -> [[Number]]
reduce xs = splitNumbers xs 2 >>= (\(taken, nontaken) -> [r : nontaken | r <- calc2 taken])

splitNumbers :: [Number] -> Int -> [([Number], [Number])]
splitNumbers xs c = [ (taken, xs \\ taken) | taken <- comb xs c]

comb :: [a] -> Int -> [[a]]
comb _ 0 = [[]]
comb xs c
    | length xs <= c = [xs]
    | otherwise = nontaken ++ fmap (head xs :) taken
        where nontaken = comb (tail xs) c
              taken = comb (tail xs) (c - 1)

calc2 :: [Number] -> [Number]
calc2 [Number x xstr, Number y ystr] = [Number (x+y) (printf "(%s+%s)" xstr ystr), 
    Number (x*y) (printf "(%s*%s)" xstr ystr), 
    Number (x-y) (printf "(%s-%s)" xstr ystr),
    Number (y-x) (printf "(%s-%s)" ystr xstr),
    Number (x/y) (printf "(%s/%s)" xstr ystr),
    Number (y/x) (printf "(%s/%s)" ystr xstr)]
calc2 _ = []
