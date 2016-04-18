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

interl :: a -> [a] -> [[a]]
interl a [] = [[a]]
interl a (x: xs) =
  (a:x:xs) : map (x:) (interl a xs)

permu :: [a] -> [[a]]
permu [] = [[]]
permu [x] = [[x]]
permu (x:xs) = concat (map (interl x) (permu xs))


partit :: [a] -> [[[a]]]
partit [] = [[[]]]
partit [x] = [[[x]]]
partit (x:xs) = [(x: head l):(tail l) | l <- partit xs] ++ [[x]:l | l <- partit xs]


-- generic data type
data Stack a = Stack [a]
  deriving (Eq, Show, Ord)

empty :: Stack a
empty =  Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop   (Stack (x:xs)) = (x, Stack (xs))

isEmpty :: Stack a -> Bool
isEmpty (Stack (xs)) = length xs == 0

depth :: Stack a -> Int
depth (Stack (xs)) = length xs



