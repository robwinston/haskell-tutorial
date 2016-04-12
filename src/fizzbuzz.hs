module FizzBuzz where

fizzbuzz :: Int -> String
fizzbuzz x
  | mod x 3 == 0 && mod x 5 == 0 = "Fizz Buzz"
  | mod x 3 == 0 = "Fizz"
  | mod x 5 == 0 = "Buzz"
  | otherwise = show x

fizzbuzzes xs = map fizzbuzz xs

