module Lygg.Ch01 where

-- functions are building blocks ...
doubleMe x  = x * 2
doubleUs x y = doubleMe x + doubleMe y


doubleSmallNumberIF1 x = if x > 100
                        then x
                        else x*2

-- illustrates that 'if' is an expression
doubleSmallNumberIF2 x = (if x > 100 then x else x*2) + 1

-- using pattern matching instead (discussed later actually)
doubleSmallNumberPM x
  | x > 100 = x
  | otherwise = x*2


-- special characters permitted
conanO'Brien = "It's a-me, Conan O'Brien!"

-- list comprehensions

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- nested list comprehension
evens1 :: [[Int]] -> [[Int]]
evens1 xxs = [ [ x | x <- xs, even x ] | xs <- xxs]

evens2 :: [[Int]] -> [Int]
evens2 xxs = concat [ [ x | x <- xs, even x ] | xs <- xxs]


triples = [ (a,b,c) | c <- [1..10], a <- [1..10], b <- [1..10] ]
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
rt1 = [ (a,b,c) | (a,b,c) <- triples, a^2 + b^2 == c^2]

-- interdependent limits filters:
--   impossibles (side longer than hypotenuse)
--   duplicates a,b == b,a
-- [(4,3,5),(8,6,10)]
rt2 = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2]

-- compoound condition
rt3 = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c==24]

-- infinite list
rt4 = [ (a,b,c) | c <- [1..], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2]



