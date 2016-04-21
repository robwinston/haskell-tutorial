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

partit :: [a] -> [[[a]]]
partit [] = [[[]]]
partit [x] = [[[x]]]
partit (x:xs) = [(x: head l):(tail l) | l <- partit xs] ++ [[x]:l | l <- partit xs]


-- generic data type
data Stack a = Stack [a]
  deriving (Eq, Ord)

instance (Show a) => Show (Stack a)
    where show (Stack l) = printElems l

emptyS :: Stack a
emptyS =  Stack []

push :: Stack a -> a -> Stack a
push (Stack xs)  x = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop   (Stack (x:xs)) = (x, Stack (xs))

isEmpty :: Stack a -> Bool
isEmpty (Stack (xs)) = length xs == 0

depth :: Stack a -> Int
depth (Stack (xs)) = length xs

sumStack :: (Num a) => Stack a -> a
sumStack (Stack (xs)) = foldr (+) 0 xs

-- frivolous function to play with syntax
-- prepends reversed stack to supplied list
popper :: ([a], Stack a) -> ([a], Stack a)
popper (es, (Stack (xs)))
 | isEmpty (Stack (xs)) = (es, emptyS)
 | otherwise = popper (e:es, ns)
   where (e, ns) = pop (Stack (xs))



printElems :: (Show a) => [a] -> String
printElems [] = ""
printElems [x] = show x
printElems (x:xs) = show x ++ "->" ++ printElems xs

