module Gfp.Lect02 where

-- verifying basic pattern matching & recursion
square :: Int -> Int
square x = x * x

squareL :: [Int] -> [Int]
squareL [] = []
squareL (x:xs) = (square x):(squareL xs)

{-
ghci> squareL [1..10] == map square [1..10]
True
-}



nonTerm :: Int -> Int
nonTerm x = nonTerm (x+1)

three :: Int -> Int
three x = 3

{-
nonTerm is non-terminating, but:
ghci> three (nonTerm 4)
3
because lazy evaluation means "nonTerm 4" never has to be evaluated
-}
