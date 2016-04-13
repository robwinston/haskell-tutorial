module Sieve where
sieve :: Int -> [Int]
sieve n = sieveToUntil n (sieveSize n)

-- find primes up to ceiling, stop if get limit
sieveToUntil :: Int -> Int -> [Int]
sieveToUntil limit ceiling =  fst (factorOutShiftUntil limit ([], [2..ceiling]))

-- estimate sieve size, from how many primes are asked for
sieveSize :: Int -> Int
sieveSize n = fromIntegral (truncate ((logBase 2 numN) * numN))
  where numN = fromIntegral n

-- for 1st element in second list ->
-- remove all of its multiples from this list
-- append it to first list
factorOutShiftUntil :: Int -> ([Int], [Int]) -> ([Int], [Int])
factorOutShiftUntil limit xt
 | length toDo == 0 = xt
 | length done >= limit = xt
 | (head toDo) * (head toDo)  > last toDo = (fst xt ++ take (limit - (length done)) toDo, [])
 | otherwise = factorOutShiftUntil limit ((fst xt) ++ (fst xtNew), (snd xtNew))
 where
   done = fst xt
   toDo = snd xt
   xtNew = factorOutFirst toDo

factorOutFirst :: [Int] -> ([Int], [Int])
factorOutFirst (x:xs) = ([x] , factorOut x xs)

-- remove all of the multiples of n from xs
factorOut :: Int -> [Int] -> [Int]
factorOut n xs
  | n < 2 = xs
  | otherwise = filter (\y -> y < n || mod y n /= 0) xs

