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


