module MyLists where

-- 1st duff attempt, does it but backwards
initSegsRev :: [a] -> [[a]]
initSegsRev [] = [[]]
initSegsRev l = l : initSegsRev (init l)

-- in effect achieves this:
-- []: map (1:) ... ([]: map (n-1:) ([]: map (n:) [[]] ))
initSegs :: [a] -> [[a]]
initSegs [] = [[]]
initSegs (x:xs) = [] : map (x:) (initSegs xs)

interleave :: a -> [a] -> [[a]]
interleave a [] = [[a]]
interleave a (x: xs) =
  (a:x:xs) : map (x:) (interleave a xs)

permute :: [a] -> [[a]]
permute [] = [[]]
permute [x] = [[x]]
permute (x:xs) = concat (map (interleave x) (permute xs))


partition :: [a] -> [[[a]]]
partition [] = [[[]]]
partition [x] = [[[x]]]
partition (x:xs) = [(x: head l):(tail l) | l <- partition xs] ++ [[x]:l | l <- partition xs]

