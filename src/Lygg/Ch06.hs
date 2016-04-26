module Lygg.Ch06 where

import Lygg.Geometry
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as Map

-- break string up into words, sort them, group them -> return list of ("word", count)
wordNums :: String -> [(String,Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

-- successively generate tails - if one of them starts with needle, bingo
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

-- simple shift encode / decode
encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg


-- the first natural number such that the sum of its digits equals (my solution)
sumDigits :: Int -> (Int, Int)
sumDigits val = fromJust $ find (\(n,s) -> n == val) $ zip (map sum $ map (map digitToInt) (map show [1..])) [1..]


phoneBookL =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

-- "*** Exception: Prelude.head: empty list" if key not found
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

-- solves problem with a Maybe
findKeyM :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeyM key xs
  | not $ null $ hits = Just $ snd $ head hits
  | otherwise = Nothing
  where hits = filter (\(k, v) -> key == k) $ xs


--  and a fold
findKeyF :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeyF key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

phoneBookM = Map.fromList phoneBookL



phoneBookLD =
    [("betty", "555-2938")
    ,("betty", "342-2492")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("patsy", "943-2929")
    ,("patsy", "827-9162")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ,("penny", "555-2111")
    ]

-- function to handle duplicates
phoneBookToMap1 :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap1 xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2


-- alt solution: make all the values in the association list singleton lists and then use ++ to combine the numbers:
phoneBookToMap2 :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap2 xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

