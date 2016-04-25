module Lygg.Ch04 where

max' :: (Ord a) => [a] -> a
max' [] = error "maximum of empty list!"
max' [x] = x
max' (x:xs) = max x (max' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem4 :: (Eq a) => a -> [a] -> Bool
elem4 a [] = False
elem4 a (x:xs)
    | a == x    = True
    | otherwise = a `elem4` xs

-- (dedupe $ quicksort "the quick brown fox jumps over the lazy dog") == ' ':['a'..'z'] => True

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

dedupe :: (Ord a) => [a] -> [a]
dedupe [] = []
dedupe (x:xs)
 | length xs == 0 = [x]
 | x == head xs = dedupe xs
 | otherwise = x : dedupe xs

