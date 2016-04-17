module Sieve where
sieve :: (Integral a) => a -> [a]
sieve n = sieveToUntil n (sieveSize n)

-- find primes up to ceiling, stop if get limit
sieveToUntil :: (Integral a) => a -> a -> [a]
sieveToUntil limit ceiling =  fst (factorOutShiftUntil limit ([], [2..ceiling]))

-- estimate sieve size, from how many primes are asked for
sieveSize ::  (Integral a) => a -> a
sieveSize n = truncate ((logBase 2 numN) * numN)
  where numN = fromIntegral n

-- for 1st element in second list ->
-- remove all of its multiples from this list
-- append it to first list
factorOutShiftUntil :: (Integral a) => a -> ([a], [a]) -> ([a], [a])
factorOutShiftUntil l xt
 | toDo == [] = xt
 | length done >= limit = xt
 | (head toDo) * (head toDo)  > last toDo = (done ++ take (limit - (length done)) toDo, [])
 | otherwise = factorOutShiftUntil l (done ++ (fst xtNew), (snd xtNew))
 where
   limit = fromIntegral l
   done = fst xt
   toDo = snd xt
   nextOne = head toDo
   xtNew = ([nextOne], factorOut nextOne toDo)

-- remove f and its multiples from l
factorOut :: (Integral a) => a -> [a] -> [a]
factorOut f l = [x | x <- l, mod x f /=0]

