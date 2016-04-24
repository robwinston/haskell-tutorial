module TicTacAltStrategy where

import TicTacCore

import Data.List
import Data.Maybe
import Data.Ord
import  qualified Data.Set as DS



-- / game strategy

-- given a board, try to make best next move
-- streamlined strategy ... this will autoplay every starting location to a draw
-- but can be defeated by a human with certain sequences
smarterMove :: Board -> Board
smarterMove brd
    | isJust loc = makeMove brd (fromJust loc)
    | otherwise = brd
    where  ply = whosMove brd
           loc = betterUnplayedSquare brd ply

-- from a list of squares, return the 1st unticked one of highest rank or 0
-- using more involved ranking
betterUnplayedSquare :: Board -> Player -> Maybe Location
betterUnplayedSquare brd ply
 | length possibleLocations == 0 = Nothing
 | otherwise = Just (head possibleLocations)
 where possibleLocations = rankUnplayedLocations brd ply

-- returns unplayed positions ranked by most tics for player in its intersections
rankUnplayedLocations :: Board -> Player -> [Location]
rankUnplayedLocations brd ply =
 map nexus (sortBy (rankIntersectionFor ply) (unplayedIntersections brd))

-- if one intersection has a better score, it's better
-- if they're the same, rank by their location
-- ... all descending, which is why they look backwards
-- avoids (albeit minor in this case) reverse expense ... because I never want them ascending
rankIntersectionFor :: Player -> Intersection -> Intersection -> Ordering
rankIntersectionFor ply i1 i2
  | i1Score > i2Score = LT
  | i1Score < i2Score = GT
  | i1Score == i2Score && i1Nexus > i2Nexus = LT
  | i1Score == i2Score && i1Nexus < i2Nexus = GT
  | otherwise = EQ
  where i1Score = scoreIntersectionFor ply i1
        i2Score = scoreIntersectionFor ply i2
        i1Nexus = nexus i1
        i2Nexus = nexus i2

scoreIntersectionFor :: Player -> Intersection -> Int
scoreIntersectionFor ply i
 -- winner or loser, easy choice
 | elem Winner scoresMe = 34
 | elem Loser scoresMe = 32
 -- force for me
 | length myNextTos > 1 = 30
 -- magic square for me
 | (length $ filter (== ForkableMe) scoresMe) > 1 = 28
 -- force for opponent
 | length opNextTos > 1 = 26
 -- magic square for opponent
 | (length $ filter (== ForkableOther) scoresMe) > 1 = 24
 -- it's an open centre
 | unblocked && theCentre itsNexus  = 10
 -- it's open corner & opponent occupies opposite
 | unblocked && aCorner itsNexus && tic (squareAt itsOpposite i) == opy = 20
 -- it's an open corner
 | unblocked && aCorner itsNexus  = 6
 -- it possess some other advantage ...
 | or $ map (> Playable) scoresMe = 4
 -- well, it isn't blocked at least
 | unblocked = 2
 -- we're on our way to a draw
 | otherwise = 0
  where itsNexus = nexus i
        scoredMe = map (scoreSqListFor ply) (rows i)
        scoresMe = map fst scoredMe
        scoredOp = map (scoreSqListFor opy) (rows i)
        scoresOp = map fst scoredOp
        unblocked = or $ map (> Blocked) scoresMe
        itsOpposite = opposite itsNexus  -- "opposite location"
        opy = otherPlayer ply
        itsNextTos = nextTosi i
        myNextTos = filter (\sq -> tic sq == ply) itsNextTos
        opNextTos = filter (\sq -> tic sq == opy) itsNextTos

-- index a square out of an intersection's "rows"
-- caller expected to know it will be there, so doesn't protect against empty list
squareAt :: Location -> Intersection -> Square
squareAt  loc i = head $ filter (\sq -> location sq == loc) sqs
  where sqs = concat (rows i)

-- "next to" == adjacent && (in a winning sequence) - e.g. diagonals are only "next to" corners or centre

allNextTos :: [Location] -> [Square] -> [(Location, [Square])]
allNextTos _ [] = []
allNextTos ls sqs =  [(loc, (nextTos sqs loc)) | loc <- ls]

-- retrieve a list of squares "next to" an intersection
nextTosi :: Intersection -> [Square]
nextTosi i = nextTos (concat $ rows i) (nexus i)

-- retireve a list of squares "next to" a location from supplied list of squares
nextTos :: [Square] -> Location -> [Square]
nextTos [] _ = []
nextTos sqs loc = filter (\sq -> nextTo loc (location sq)) sqs

-- are squares "next to"  one another?
nextToSq :: Square -> Square -> Bool
nextToSq sq1 sq2 = nextTo (location sq1) (location sq2)

-- are locations "next to"  one another?
nextTo :: Location -> Location -> Bool
nextTo l1 l2 = elem l2 (adjacentLocations l1)

-- perhaps these nextTo / adjacentCorners could be computed, but pattern match is easy

--   for "rows" of squares ... logic makes no sense if this is a random collection of squares
scoreSqListFor :: Player -> [Square] -> (Score, [Square])
scoreSqListFor ply sqs
  | players + opponents == sqsLength = (Unplayable, sqs)   -- played out
  | players == sqsLength - 1 = (Winner, sqs)               -- a winner
  | opponents == sqsLength - 1 = (Loser, sqs)              -- a loser
  | players > 0 && opponents > 0 = (Blocked, sqs)          -- blocked
  | players > 0 && opponents == 0 = (ForkableMe, sqs)      -- press a claim
  | players == 0 && opponents > 0 = (ForkableOther, sqs)   -- press a claim
  | opponents == 0 = (MaybeMe, sqs)                        -- stake a claim
  | players == 0 = (MaybeOther, sqs)                       -- negate a claim
  | otherwise = (Playable, sqs)                            -- doesn't really matter
  where sqsLength = length sqs
        players = ticCount ply sqs
        opponent = otherPlayer ply
        opponents = ticCount opponent sqs

-- / game strategy

-- / simple game strategy

smartMove :: Board -> Board
smartMove brd
  | isJust loc = makeMove brd (fromJust loc)
  | otherwise = brd
  where loc = pickUnplayedSquare $ head $ rankBoardRows brd (whosMove brd)

-- from a list of squares, return location of the 1st unticked one
-- using fairly simple ranking
pickUnplayedSquare :: [Square] -> Maybe Location
pickUnplayedSquare squares
  | length sqs == 0 = Nothing
  | otherwise = Just (location $ head $ rankSquares sqs)
  where sqs = filter isUnplayed squares


-- order "rows" by how 'good' they are for Player
rankBoardRows :: Board -> Player ->  [[Square]]
rankBoardRows brd ply = sortBy  (rankSqList ply) (playableRows brd)


-- by score ... descending (compare is backwards)
rankSqList :: Player -> [Square] -> [Square] -> Ordering
rankSqList ply first second
  | score1st > score2nd = LT
  | score1st < score2nd = GT
  | otherwise = EQ
  where score1st = fst $ scoreSqListFor ply first
        score2nd = fst $ scoreSqListFor ply second

--- order squares by "rank" descending
rankSquares :: [Square] -> [Square]
rankSquares squares = sortBy rankSquare squares

rankSquare :: Square -> Square -> Ordering
rankSquare sq1 sq2 = rankLocation (location sq1) (location sq2)

-- \ simple game strategy

