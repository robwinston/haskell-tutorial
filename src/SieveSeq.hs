module SieveSeq where

import Sieve
import Data.Sequence
import Data.Foldable (toList)

sieveSq :: (Integral a) => a -> [a]
sieveSq n = sieveToUntilSq n (sieveSize n)

-- find primes up to ceiling, stop if get limit
sieveToUntilSq :: (Integral a) => a -> a -> [a]
sieveToUntilSq limit ceiling =  fst (fosusl limit ([], [2..ceiling]))


-- List wrapper for factorOutShiftUntilSeq
fosusl :: (Integral a) => a -> ([a], [a]) -> ([a], [a])
fosusl limit xt =  toListTuple (factorOutShiftUntilSeq limit (fromListTuple xt))


-- for 1st element in second list ->
-- remove all of its multiples from this list
-- append it to first list
factorOutShiftUntilSeq :: (Integral a) => a -> (Seq a, Seq a) -> (Seq a, Seq a)
factorOutShiftUntilSeq limit xt
 | toDo == empty = xt
 | Data.Sequence.length done >= limiti = xt
 | nextOne * nextOne  > lastOne = (done >< Data.Sequence.take (limiti - (Data.Sequence.length done)) toDo, empty)
 | otherwise = factorOutShiftUntilSeq limit (done >< (fst xtNew), (snd xtNew))
 where
   limiti = fromIntegral limit
   done = fst xt
   toDo = snd xt
   nextOne = index toDo 0
   lastOne = index toDo ((Data.Sequence.length toDo)-1)   -- can't find Sequence equiv of last
   xtNew = (singleton nextOne, factorOutSq nextOne toDo)


-- remove f and its multiples from l
factorOutSq :: (Integral a) => a -> Seq a -> Seq a
-- factorOutSq f l = [x | x <- l, mod x f /=0]
factorOutSq f l = Data.Sequence.filter (\x -> mod x f /=0) l


toListTuple :: (Seq a, Seq b) -> ([a], [b])
toListTuple (a,b) = (toList a, toList b)

fromListTuple :: ([a], [b]) -> (Seq a, Seq b)
fromListTuple (a,b) = (fromList a, fromList b)
