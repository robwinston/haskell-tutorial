module Misc.Math where

factorial :: Integer -> Integer
factorial n
  | n == 0 = 1
  | n > 0 = n * factorial (n -1)
  | otherwise = factorial (-n)


mygcd :: (Integral a) => a -> a -> a
mygcd a 0 = a
mygcd a b
  | a >= b = mygcd b (mod a b)
  | otherwise = mygcd b a


isPrime :: (Integral a) => a -> Bool
isPrime x
  | x == 1 = False
  | otherwise = not (hasDivisor x (div x 2))

hasDivisor :: (Integral a) => a  -> a -> Bool
hasDivisor x y
  | y < 2  = False
  | mod x y == 0 = True
  | otherwise = hasDivisor x (y-1)

-- list comprehension basics
divisors :: (Integral a) => a  -> [a]
divisors n
  | n < 0 = divisors (-n)
  | otherwise = [x | x <- [1..((quot n 2)+1)], mod n x == 0]

primeDivisors :: (Integral a) => a  -> [a]
primeDivisors n = [x | x <- divisors n, isPrime x ]

primes :: (Integral a) => [a]
primes = thisSieve [2..]
  where thisSieve (x:xs) = x:thisSieve [y| y <- xs, mod y x > 0]

-- pythagorian triples
pytriples :: (Integral a) => a -> [(a,a,a)]
pytriples limit = [(x,y,z) | x <- [1..limit], y <- [(x+1)..limit], z <- [(y+1)..limit], x*x + y*y == z*z]


-- prime factorization
pfs :: (Integral a) => a -> [a]
pfs n
  | n < 0 = (-1) : pfs (abs n)
  | n == 0 = []
  | n == 1 = [1]
  | otherwise = np : pfs (whatsLeft n np)
  where np = nextPf n

nextPf :: (Integral a) => a -> a
nextPf n
  | pds == [] = n
  | otherwise = head (pds)
  where pds = primeDivisors n

whatsLeft :: (Integral a) => a -> a -> a
whatsLeft n f
 | n == f = 0
 | otherwise = quot n f


nondecreasing :: (Ord a) => [a] -> Bool
nondecreasing xs = and [ x <= y |  (x,y) <- zip xs (tail xs) ]

multiples :: (Integral a) => a -> [a]
multiples n = [x | x <- [1..], mod x n == 0]

-- insert value into an already sorted list
insert :: (Ord a) => a -> [a] -> [a]
insert y [] =  [y]
insert y (x:xs)
  | y <= x = y:(x:xs)
  | otherwise = x:(insert y xs)

isort :: (Ord a) => [a] -> [a]
isort [] = []
isort [a] = [a]
isort (x:xs) = insert x (isort xs)

isortf :: (Ord a) => [a] -> [a]
isortf l = foldr insert [] l

bsort :: (Ord a) => [a] -> [a]
bsort [] = []
bsort xs
 | length xs < 10 = isort xs
 | otherwise = mergeSorted (bsort leftHalf) (bsort rightHalf)
     where (leftHalf, rightHalf) = halveList xs


qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (pivot:xs) = (qsort lower) ++ (pivot : (qsort higher))
  where lower = [x| x<- xs, x < pivot]
        higher = [x| x <- xs, x > pivot]


mergeSorted :: (Ord a) => [a] -> [a] -> [a]
mergeSorted [] rs = rs
mergeSorted ls [] = ls
mergeSorted (l:ls) (r:rs)
  | l <= r = l : mergeSorted ls (r:rs)
  | otherwise = r : mergeSorted (l:ls) rs

halveList :: [a] -> ([a],[a])
halveList [] = ([],[])
halveList xs = splitAt midpt xs
  where midpt = quot (length xs) 2


