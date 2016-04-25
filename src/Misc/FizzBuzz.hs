module Misc.FizzBuzz where

fizzbuzz :: Int -> String
fizzbuzz x
  | mod x 3 == 0 && mod x 5 == 0 = "Fizz Buzz"
  | mod x 3 == 0 = "Fizz"
  | mod x 5 == 0 = "Buzz"
  | otherwise = show x

fizzbuzzes xs = map fizzbuzz xs

fbpairs xs = zip xs (fizzbuzzes xs)

-- sample manipulate pair elements
-- filter ((\a -> mod a 5 == 0).fst) (fbpairs [1..45])