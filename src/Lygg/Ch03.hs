module Lygg.Ch03 where

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


bmiTell :: Double -> Double -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, eat more!"
    | weight / height ^ 2 <= 25.0 = "Looking good!"
    | weight / height ^ 2 <= 30.0 = "You're overweight! Let's work out together!"
    | otherwise                   = "You're obese. Go see a doctor."


tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:xs) = "This list has " ++  wordnum (length xs + 2) ++ " entries. The first two elements are: " ++ show x ++ " and " ++ show y
  where wordnum :: Int -> String
        wordnum n
          | n < 10 = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! (n-1)
          | otherwise = show n

-- let is a bit different ...
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

-- let in a list comprehension
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- add a predicate
calcBmisAbove :: Double -> [(Double, Double)] ->  [Double]
calcBmisAbove floor xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > floor]

-- case v pattern match

describeListC :: [a] -> String
describeListC ls = "The list is " ++ case ls of [] -> "empty."
                                                [x] -> "a singleton list."
                                                xs -> "a longer list."


describeListW :: [a] -> String
describeListW ls = "The list is " ++ sz ls
    where sz [] = "empty."
          sz [x] = "a singleton list."
          sz xs = "a longer list."