module MyMath where

factorial :: Integer -> Integer
factorial n
  | n == 0 = 1
  | n > 0 = n * factorial (n -1)
  | otherwise = factorial (-n)


mygcd :: Int -> Int -> Int
mygcd a 0 = a
mygcd a b
  | a >= b = mygcd b (mod a b)
  | otherwise = mygcd b a


isPrime :: Int -> Bool
isPrime x
  | x == 1 = False
  | otherwise = not (hasDivisor x (div x 2))

hasDivisor :: Int -> Int -> Bool
hasDivisor x y
  | y < 2  = False
  | mod x y == 0 = True
  | otherwise = hasDivisor x (y-1)

-- list comprehension basics
divisors :: Int -> [Int]
divisors n
  | n < 0 = divisors (-n)
  | otherwise = [x | x <- [1..((quotInt n 2)+1)], mod n x == 0]

quotInt dividend divisor  = fromIntegral (quot dividend divisor)

primeDivisors n = [x | x <- divisors n, isPrime x ]

primes :: [Int]
primes = thisSieve [2..]
  where thisSieve (x:xs) = x:thisSieve [y| y <- xs, mod y x > 0]

-- pythagorian triples
pytriples limit = [(x,y,z) | x <- [1..limit], y <- [(x+1)..limit], z <- [(y+1)..limit], x*x + y*y == z*z]


-- prime factorization
pfs :: Int -> [Int]
pfs n
  | n < 0 = (-1) : pfs (abs n)
  | n == 0 = []
  | n == 1 = [1]
  | otherwise = np : pfs (whatsLeft n np)
  where np = nextPf n

nextPf :: Int -> Int
nextPf n
  | pds == [] = n
  | otherwise = head (pds)
  where pds = primeDivisors n

whatsLeft :: Int -> Int -> Int
whatsLeft n f
 | n == f = 0
 | otherwise = fromIntegral (quot n f)


nondecreasing :: [Int] -> Bool
nondecreasing xs = and [ x <= y |  (x,y) <- zip xs (tail xs) ]


multiples n = [x | x <- [1..], mod x n == 0]

-- insert value into an already sorted list
insert :: Int -> [Int] -> [Int]
insert y [] =  [y]
insert y (x:xs)
  | y <= x = y:(x:xs)
  | otherwise = x:(insert y xs)

isort :: [Int] -> [Int]
isort [] = []
isort [a] = [a]
isort (x:xs) = insert x (isort xs)

isortf :: [Int] -> [Int]
isortf l = foldr insert [] l

bsort :: [Int] -> [Int]
bsort [] = []
bsort xs
 | length xs < 10 = isort xs
 | otherwise = mergeSorted (bsort leftHalf) (bsort rightHalf)
     where (leftHalf, rightHalf) = halveList xs


qsort :: [Int] -> [Int]
qsort [] = []
qsort [a] = [a]
qsort (pivot:xs) = (qsort lower) ++ (pivot : (qsort higher))
  where lower = [x| x<- xs, x < pivot]
        higher = [x| x <- xs, x > pivot]


mergeSorted :: [Int] -> [Int] -> [Int]
mergeSorted [] rs = rs
mergeSorted ls [] = ls
mergeSorted (l:ls) (r:rs)
  | l <= r = l : mergeSorted ls (r:rs)
  | otherwise = r : mergeSorted (l:ls) rs

halveList :: [a] -> ([a],[a])
halveList [] = ([],[])
halveList xs = splitAt midpt xs
  where midpt = quot (length xs) 2


edge :: Char -> Char -> Bool
edge 'A' 'B' = True
edge 'A' 'D' = True
edge 'B' 'C' = True
edge 'C' 'A' = True
edge 'C' 'E' = True
edge 'D' 'E' = True
edge 'F' 'D' = True
edge 'F' 'E' = True
edge _ _ = False




connected :: Char -> Char -> Bool
connected a b
  | edge a b = True
  | length itsEdges == 0 = False
  | otherwise = length [e | e <- itsEdges, connected e b] > 0
    where itsEdges = edges a


edges :: Char -> [Char]
edges a = [e | e <- ['A'..'Z'], edge a e]