module Lygg.Ch02 where

import Data.Char
import Numeric

-- Types & Functions signatures

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- partial application (not discussed yet, but trying it)
addTwoTo :: Int -> (Int -> Int -> Int)
addTwoTo  x = addThree x

addTriple :: (Int, Int, Int) -> Int
addTriple (x,y,z) = addThree x y z


factorial :: Integer -> Integer
factorial n = product [1..n]

-- circumference 5  -> 31.415928
circumference :: Float -> Float
circumference r = 2 * pi * r

-- circumference' 5 -> 31.41592653589793
circumference' :: Double -> Double
circumference' r = 2 * pi * r


-- special characters ...
(£) :: Float -> Float
(£) p = p * 0.70

-- very silly, but trying syntactic combos / pattern matching
(!$) :: [a] -> [a] -> [a]
(!$) [] [] = []
(!$) (x:xs) [] = xs ++ [x]
(!$) [] (y:ys) = ys ++ [y]
(!$) (x:xs) (y:ys) = [y] ++ xs  ++ ys ++  [x]

-- trying  read & more pattern matching (but getting ahead of book with a Maybe)
stringToInt :: String -> Maybe Int
stringToInt [] = Nothing
stringToInt s
  | (head s) == '-' && allDigits (tail s) = Just (read s)  --  (read can handle a leading minus, but not a leading +)
  | allDigits s = Just (read s)
  | otherwise = Nothing
  where allDigits :: String -> Bool
        allDigits s = and (map (\c -> elem c ['1'..'9']) s)

hexStringToInt :: String -> Maybe Integer
hexStringToInt [] = Nothing
hexStringToInt s
  | (head s) == '-' && allHexDigits (tail s) = Just ((-1) * (fst $ head $ (readHex (tail s))))
  | allHexDigits s = Just (fst $ head $ (readHex s))
  | otherwise = Nothing
  where allHexDigits :: String -> Bool
        allHexDigits s = and (map (\c -> elem c (['1'..'9']++['A'..'F'])) s)


--trying custom enum, using stuff not yet covered ...

-- seems a naked digit isn't allowed in an enum?
data Rank = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | J | Q | K | A
  deriving (Eq, Ord, Enum, Bounded)


instance Show Rank
  where show r = showRank (r)
showRank :: Rank -> String
showRank r = (map show [2..10] ++ ["J","Q","K","A"]) !! (fromEnum r)


-- absent a standard, adopting suit ranking used in bridge bidding
data Suit = C | D | H | S
  deriving (Eq, Ord, Show, Enum, Bounded)

data Card = Card {
                 rank :: Rank,
                 suit :: Suit }
              deriving (Eq, Ord)

-- "as" pattern e.g. "card@" is introduced in chapter 3
instance Show Card
  where show card@(Card r s) = showCard (card)
showCard :: Card -> String
showCard (Card {rank = r, suit = s}) = (show r) ++ (show s)

