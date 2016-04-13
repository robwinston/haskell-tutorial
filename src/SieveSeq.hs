module SieveSeq where

import Sieve
import Data.Sequence
import Data.Foldable

sieveSq :: Int -> [Int]
sieveSq n = sieveToUntilSq n (sieveSize n)

-- find primes up to ceiling, stop if get limit
sieveToUntilSq :: Int -> Int -> [Int]
sieveToUntilSq limit ceiling =  fst (fosusl limit ([], [2..ceiling]))


-- List wrapper for factorOutShiftUntilSeq
fosusl :: Int -> ([Int], [Int]) -> ([Int], [Int])
fosusl limit xt =  toListTuple (factorOutShiftUntilSeq limit (fromListTuple xt))


-- for 1st element in second list ->
-- remove all of its multiples from this list
-- append it to first list
factorOutShiftUntilSeq :: Int -> (Seq Int, Seq Int) -> (Seq Int, Seq Int)
factorOutShiftUntilSeq limit xt
 | toDo == empty = xt
 | Data.Sequence.length done >= limit = xt
 | nextOne * nextOne  > lastOne = (done >< Data.Sequence.take (limit - (Data.Sequence.length done)) toDo, empty)
 | otherwise = factorOutShiftUntilSeq limit (done >< (fst xtNew), (snd xtNew))
 where
   done = fst xt
   toDo = snd xt
   nextOne = index toDo 0
   lastOne = index toDo ((Data.Sequence.length toDo)-1)   -- can't find Sequence equiv of last
   xtNew = (singleton nextOne, fosq nextOne toDo)


-- Data.Sequence wrapper for factorOut
fosq :: Int -> Seq Int -> Seq Int
fosq i l = fromList (factorOut i (toList l))


toListTuple :: (Seq a, Seq b) -> ([a], [b])
toListTuple (a,b) = (toList a, toList b)

fromListTuple :: ([a], [b]) -> (Seq a, Seq b)
fromListTuple (a,b) = (fromList a, fromList b)
