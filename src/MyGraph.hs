module MyGraph where

import Data.List (nub)


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


connexions :: Char -> [Char]
connexions a = filter (connected a) ['A'..'Z']

connected :: Char -> Char -> Bool
connected a b
  | edge a b = True
  | length possibleRoutes == 0 = False
  | length (filter (== b) (map snd possibleRoutes)) == 0 = False
  | otherwise = True
    where possibleRoutes = routesFor (edgePairs a)


route :: Char -> Char -> [(Char, Char)]
route a b
  | edge a b = [(a,b)]
  -- next two patterns: if there's simply no way to get from a to b, don't bother tracing
  | length possibleRoutes == 0 = []
  | length (filter (== b) (map snd possibleRoutes)) == 0 = []
  -- this is obviously not finished ...
  | otherwise = possibleRoutes
    where eps = edgePairs a
          possibleRoutes = routesFor (eps)


routesFor :: [(Char, Char)] ->  [(Char, Char)]
routesFor rts
  | length notTried == 0 = rts
  | otherwise = routesFor (rts ++ concat (map edgePairs notTried))
  where notTried = untried rts

edges :: Char -> [Char]
edges a = [e | e <- ['A'..'Z'], edge a e]

edgePairs :: Char -> [(Char,Char)]
edgePairs a = [(a,e) | e <- ['A'..'Z'], edge a e]

allPathsFromTo :: Char ->  Char -> [[Char]]
allPathsFromTo f t = [np | np <- (allPathsFor f), last np == t]

allPathsFor :: Char ->  [[Char]]
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

nextPaths :: [Char]-> [[Char]]
nextPaths [] = []
nextPaths path
  | isCircular path = []
  | length todo == 0 = []
  | otherwise = [path++[e]| e <- (edges (last path))]
  where todo = edges (last path)


isCircular :: [Char] -> Bool
isCircular [c] = False
isCircular p = head p == last p

-- given a char and a list of char pairs,
-- return all char pairs where char is first element
nextSteps :: Char -> [(Char, Char)] -> [(Char, Char)]
nextSteps s ps = filter (\p -> fst p == s) ps

-- given a list of edge pairs,
-- return all second elements which are not also first elements
-- & are connected to something, i.e. aren't dead ends
untried :: [(Char, Char)] -> [Char]
untried cps = filter (\u -> length (edges u) > 0) (nub [c | c <- map snd cps, not (elem c (map fst cps))])

maxLength :: [[a]] -> Int
maxLength l =  maximum $ map length l

longestOnly :: [[a]] -> [[a]]
longestOnly [[]] = [[]]
longestOnly ps = [p | p <- ps, length p == maxLength ps]

