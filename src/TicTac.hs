module TicTac where

import TicTacCore

import Data.List
import Data.Maybe
import Data.Ord
import  qualified Data.Set as DS

-- / game play

play :: Board -> Int -> Board
play brd i
 | aWinner brd = brd
 | isNothing loc  = cleverMove brd
 | otherwise = playl brd (fromJust loc)
 where loc = maybeLocation i

playl :: Board -> Location -> Board
playl brd loc
  | aWinner brd = brd
  | otherwise = makeSuppliedMove brd loc



cleverMove :: Board -> Board
cleverMove brd
  | isJust whereToMove = makeMove brd (head $ fromJust whereToMove)
  | otherwise = brd
  where ply = whosMove brd
        opy = otherPlayer ply
        tests :: [(Board -> [Location])]
        tests = [openingMove, canWin ply, canWin opy, canFork ply, blocking, isUnoccupied  centre, oppositeOccupied, isUnoccupied corners, isUnoccupied middles, unplayedLocations]
        whereToMove = find (\mvs -> length mvs > 0)  (map (\f -> f brd) tests)

-- unplayed corners occupied by opponent
oppositeOccupied :: Board -> [Location]
oppositeOccupied brd = [loc | loc <- unplayedLocations brd, (elem loc corners)  && ((tic $ squareFor brd $ opposite loc) == oply)]
  where oply = otherPlayer $ whosMove brd

openingMove :: Board -> [Location]
openingMove brd
  | (length $ allLocations brd) == (length $ unplayedLocations brd) = corners
  | otherwise = []

canWin :: Player -> Board -> [Location]
canWin ply brd  =
  map fst  (filter (\(loc,t) -> t > 0) (map (\(loc,ts) -> (loc, length $ filter (\t -> countForPlayer ply t == 2) ts)) (unplayedTallys brd)))

canFork :: Player -> Board -> [Location]
canFork ply brd  =
  map fst  (filter (\(loc,t) -> t > 1) (map (\(loc,ts) -> (loc, length $ filter (\t -> countForPlayer ply t == 1 && countForPlayer opy t == 0) ts)) (unplayedTallys brd)))
  where opy = otherPlayer ply


blocking :: Board -> [Location]
blocking brd
  | not $ null $ cornerBlock = cornerBlock
  | not $ null $ forceToMiddle = forceToMiddle
  | not $ null $ forkableByOpponent = forkableByOpponent
  | not $ null $ forceableOnly = forceableOnly
  | otherwise = []
  where forceable = canForce ply brd
        forkableByOpponent = canFork opy brd
        inboth = intersect forceable forkableByOpponent
        forceableOnly = [l | l <- diffs forceable forkableByOpponent, not $ hasEmptyRow brd l]
        ply = whosMove brd
        opy = otherPlayer ply
        -- its a corner & it owns the other corner, so playing here will force oppoent to defend in middle
        --  (thereby keeping opponent from exploiting an opportunity)
        cornerBlock = [l | l <- inboth, (elem l corners) && (length (filter (\sq ->  (tic sq) == ply) (squaresFor brd (adjacentCorners l))) > 0)]

        -- these are really forceableMiddle, i.e. middleBlock ?
        forceToMiddle = [l | l <- forceable, (not $ elem l corners) && (not $ hasEmptyRow brd l)]
        -- this needs to check for an empty row, which appears to be one case where this is a bad move?
        -- hasEmptyRow :: Board -> Location -> Bool


canForce :: Player -> Board -> [Location]
canForce ply brd =
  map fst  (filter (\(loc,t) -> t > 0) (map (\(loc,ts) -> (loc, length $ filter (\t -> countForPlayer ply t == 1 && countForPlayer opy t == 0) ts)) (unplayedTallys brd)))
  where opy = otherPlayer ply

