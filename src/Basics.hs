module Basics where

-- re-invent take
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake y (x:xs)
 | y <= 0 = []
 | otherwise = x : (mytake (y-1) xs)



-- / mutually recursive functions
alternating :: (Ord a) => [a] -> Bool
alternating xs = (updown xs) || (downup xs)

downup :: (Ord a) => [a] -> Bool
downup [] = True
downup [x] = True
downup (x:y:xs) = (x > y) && updown (y:xs)

updown :: (Ord a) => [a] -> Bool
updown [] = True
updown [x] = True
updown (x:y:xs) = (x < y) && downup (y:xs)
-- \ mutually recursive functions
