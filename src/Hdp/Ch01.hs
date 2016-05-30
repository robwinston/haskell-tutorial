module Hdp.Ch01 where

import System.Random

-- stack of deferred function evals kept in memory ...
sumNonTail [] = 0
sumNonTail (x:xs) = x + (sumNonTail xs)


-- Tail recursion addresses the exorbitant use of space with non-tail-recursive processes ...
sumTail' acc [] = acc
sumTail' acc (x:xs) = sumTail' (acc + x) xs
sumTail xs = sumTail' 0 xs


-- Tail recursion is captured by the foldl function
-- foldl function expands in exactly the same way as sumTail'
foldlSum :: Num a => [a] -> a
foldlSum = foldl (+) 0

-- In contrast, foldrSum expands in the same way as sumNonTail
foldrSum :: Num a => [a] -> a
foldrSum = foldr (+) 0



{-
Type combination is also known as "product of types" and type alternation as "sum of types".
In this way, we can create an "algebra of types", with sum and product as operators, hence the name Algebraic data types.
-}

-- combination of types
type Name = String
type Age = Int
data Person = P String Int -- combination

-- composite of alternatives
data MaybeInt = NoInt | JustInt Int

maybeInts = [JustInt 2, JustInt 3, JustInt 5, NoInt]


-- Parameterize algebraic types to create generic types:

data Maybe' a = Nothing' | Just' a
  deriving (Show, Eq)

-- Algebraic data type constructors also serve as "deconstructors" in pattern matching:

fMaybe f (Just' x) = Just' (f x)
fMaybe f Nothing' = Nothing'

fMaybes = map (fMaybe (* 2)) [Just' 2, Just' 3, Nothing']

{-
To do this, had to add "deriving (Show, Eq)"  ...
ghci> filter (/= Nothing') fMaybes
[Just' 4,Just' 6]
-}


-- Composite pattern with recursive algebraic types:
data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Show, Eq)


sizeJustLeaves :: Tree a -> Int
sizeJustLeaves (Leaf x) = 1
sizeJustLeaves (Branch t u) = sizeJustLeaves t + sizeJustLeaves u

sizeAll :: Tree a -> Int
sizeAll (Leaf x) = 1
sizeAll (Branch t u) = sizeAll t + sizeAll u + 1


-- Parametric polymorphism
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs


-- Alternation-based ad-hoc polymorphism
data Shape = Circle Float | Rect Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect length width) = length * width



-- Class-based ad-hoc polymorphism
data CircleC = CircleC Float
data RectC = RectC Float Float

class ShapeC a where
  areaC :: a -> Float

instance ShapeC CircleC where
  areaC (CircleC r) = pi * r^2
instance ShapeC RectC where
  areaC (RectC length' width') = length' * width'


{-
Polymorphic dispatch and the visitor pattern ...
Define behavior for all four permutations of CustomerEvent and Customer
OOP lang would need visitor pattern for multiple dispatch
-}

data CustomerEvent = InvoicePaid Float | InvoiceNonPayment
data Customer = Individual Int | Organisation Int

payment_handler :: CustomerEvent -> Customer -> String

payment_handler (InvoicePaid amt) (Individual custId)
  = "SendReceipt for " ++ (show amt)
payment_handler (InvoicePaid amt) (Organisation custId)
  = "SendReceipt for " ++ (show amt)

payment_handler InvoiceNonPayment (Individual custId)
  = "CancelService for " ++ (show custId)
payment_handler InvoiceNonPayment (Organisation custId)
  = "SendWarning for " ++ (show custId)



generate :: StdGen -> (Int, StdGen)
generate g = random g :: (Int, StdGen)

randInts' g = (randInt, g) : (randInts' nextGen)
       where (randInt, nextGen) = (generate g)


-- this isn't very useful, needs more "wrapping" to manage IO-ness ...
main = do
  gen0 <- getStdGen
  return $ randInts' gen0


data Expr = Lit Int | Div Expr Expr

eval :: Expr -> Int
eval (Lit a) = a
eval (Div a b) = eval a `div` eval b

