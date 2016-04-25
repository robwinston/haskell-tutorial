module Lygg.Ch05 where

-- partial application with an infix
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- of course can do this with subtract directly
-- just more practice matching type signatures ...
subSomething :: Num a => a -> ( a ->  a)
subSomething n = (subtract n)


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- combines two lists, applying supplied function to each "pair"
-- this explains generic type signatures
-- the function takes a,b returns c
-- therefore, corrresponding lists supplying values must be of corresponding type
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

{-
ghci> zip [1,2,3,4,5] "hello"
[(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]
ghci> flip zip [1,2,3,4,5] "hello"          <- note how function & its params is passed "in open" because of flip's signature
[('h',1),('e',2),('l',3),('l',4),('o',5)]
ghci>
-}

largestDivisible :: Integer -> Integer -> Integer
largestDivisible ceiling divisor = head (filter p [ceiling,(ceiling-1)..])
    where p x = x `mod` divisor == 0

sumPowersForBelow :: Int -> (Int -> Bool) -> Int -> Int
sumPowersForBelow pow filt ceil =   sum (takeWhile (<ceil) (filter filt (map (^pow) [1..])))


{-
A Collatz sequence (also known as a Collatz chain) is defined as follows:
Start with any natural number.
If the number is 1, stop.
If the number is even, divide it by 2.
If the number is odd, multiply it by 3 and add 1.
Repeat the algorithm with the resulting number.
-}

-- I couldn't see the flippin' chain!
-- so I built a list backwards and reversed it ...
collatz :: Int -> [Int]
collatz n
  | n < 1 = []
  | otherwise = reverse (collatzl [n])

collatzl :: [Int] -> [Int]
collatzl [] = []
collatzl (x:xs)
  | x == 1 = x:xs
  | even x = collatzl ((quot x 2):x:xs)
  | odd x = collatzl ((x*3+1):x:xs)

-- book' solution ... so much simpler, damn!
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | n < 1 = []
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

numLongChains :: (Int -> Int -> Bool) -> Int -> Int
numLongChains compare howLong = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs `compare` howLong


-- intriguing use of foldr
elem5 :: (Eq a) => a -> [a] -> Bool
elem5 y ys = foldr (\x acc -> if x == y then True else acc) False ys

countem :: (Eq a) => a -> [a] -> Int
countem y ys = foldr (\x acc -> if x == y then  acc + 1 else acc) 0 ys
