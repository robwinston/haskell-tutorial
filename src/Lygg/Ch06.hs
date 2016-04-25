module Lygg.Ch06 where


import Data.Char
import Data.Maybe
import Data.List

-- the first natural number such that the sum of its digits equals ?
sumDigits :: Int -> (Int, Int)
sumDigits val = fromJust $ find (\(n,s) -> n == val) $ zip (map sum $ map (map digitToInt) (map show [1..])) [1..]