module MyGraph where

import Data.List (nub)

-- functions below work with this graph definition
-- a way to define graphs & functions to use any graph are a natural next step
-- (when I know enough haskell to do it)
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


allPathsFromTo :: Char ->  Char -> [Path]
allPathsFromTo f t = [np | np <- (allPathsFor f), last np == t]

allPathsFor :: Char ->  [Path]
allPathsFor c = [np | np <- allPaths, head np == c]

allPaths = [np | np <- buildPaths [a:[]| a <- ['A'..'F']], length np > 1]

buildPaths :: [[Char]] -> [[Char]]
buildPaths [[]] = []
buildPaths ps
-- struggling a bit with creating empty lists of lists v list containing one empty list
-- so concat is the defence ...
  | length (concat todo) == 0 = ps
  | otherwise = buildPaths (ps ++ (concat todo))
  where todo = map nextPaths (longestOnly ps)

nextPaths :: Path-> [[Char]]
nextPaths [] = []
nextPaths path
  | isCircular path = []
  | length todo == 0 = []
  | otherwise = [path++[e]| e <- (edges (last path))]
  where todo = edges (last path)


isCircular :: Path -> Bool
isCircular [c] = False
isCircular p = head p == last p

-- given a list of edge pairs,
-- return all second elements which are not also first elements
-- & are connected to something, i.e. aren't dead ends
untried :: [(Char, Char)] -> [Char]
untried cps = filter (\u -> length (edges u) > 0) (nub [c | c <- map snd cps, not (elem c (map fst cps))])

edges :: Char -> [Char]
edges a = [e | e <- ['A'..'Z'], edge a e]

-- helper function to add next path length to existing list of paths
-- (because didn't know about 'iterate' yet)
longestOnly :: [[a]] -> [[a]]
longestOnly [[]] = [[]]
longestOnly ps = [p | p <- ps, length p == maxLength ps]

maxLength :: [[a]] -> Int
maxLength l =  maximum $ map length l


--- variation on what instructor did, using some of my solution ...

type Path = [Char]

allExtPathsFor :: Char -> [[Char]]
allExtPathsFor c = [p | p <- allExtPaths, length p > 1 && head p == c]

-- magic number related to number of vertices in example graph
allExtPaths = concat $ take 7 extendPathss

-- my 'iterate extendPaths' in effect starts over when it runs out of paths, because I proactively remove loops
-- instructors solution lets it go on forever and worries about loops later
-- that's probably more idiomatic, beacuse "start over"  behaviour is a bit odd
-- for my approach, something like iterateWhile from https://hackage.haskell.org/package/monad-loops-0.4.3 would be useful here
-- but not comfortable installing external packages yet ....
extendPathss = iterate extendPaths []

extendPaths :: [Path] -> [Path]
extendPaths [] =  [[p] | p <- ['A'..'F']]
extendPaths lp = concat $ map extendPath lp

extendPath :: Path -> [Path]
extendPath [] = []
extendPath p
  | length itsEdges == 0 = []
  | otherwise = [p++[e] | e <- itsEdges, not $ isCircular p]
  where itsEdges = edges (last p)

-- inital noodling
connexions :: Char -> [Char]
connexions a = filter (connected a) ['A'..'Z']

connected :: Char -> Char -> Bool
connected a b
  | edge a b = True
  | length possibleRoutes == 0 = False
  | length (filter (== b) (map snd possibleRoutes)) == 0 = False
  | otherwise = True
    where possibleRoutes = routesFor (edgePairs a)

routesFor :: [(Char, Char)] ->  [(Char, Char)]
routesFor rts
  | length notTried == 0 = rts
  | otherwise = routesFor (rts ++ concat (map edgePairs notTried))
  where notTried = untried rts

edgePairs :: Char -> [(Char,Char)]
edgePairs a = [(a,e) | e <- ['A'..'Z'], edge a e]

-- given a char and a list of char pairs,
-- return all char pairs where char is first element
nextSteps :: Char -> [(Char, Char)] -> [(Char, Char)]
nextSteps s ps = filter (\p -> fst p == s) ps

