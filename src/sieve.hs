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
 | toDo == [] = xt
 | length done >= limit = xt
 | (head toDo) * (head toDo)  > last toDo = (done ++ take (limit - (length done)) toDo, [])
 | otherwise = factorOutShiftUntil limit (done ++ (fst xtNew), (snd xtNew))
 where
   done = fst xt
   toDo = snd xt
   nextOne = head toDo
   xtNew = ([nextOne], factorOut nextOne toDo)

-- remove f and its multiples from l
factorOut :: Int -> [Int] -> [Int]
factorOut f l = [x | x <- l, mod x f /=0]

