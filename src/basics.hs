module Basics where

-- re-invent take
mytake :: Int -> [Int] -> [Int]
mytake _ [] = []
mytake y (x:xs)
 | y <= 0 = []
 | otherwise = x : (mytake (y-1) xs)



-- mutually recursive functions
alternating :: [Int] -> Bool
alternating xs = (updown xs) || (downup xs)

downup :: [Int] -> Bool
downup [] = True
downup [x] = True
downup (x:y:xs) = (x > y) && updown (y:xs)

updown :: [Int] -> Bool
updown [] = True
updown [x] = True
updown (x:y:xs) = (x < y) && downup (y:xs)
-- mrf
