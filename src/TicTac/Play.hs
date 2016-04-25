module TicTac.Play where

import TicTac.Common

import Data.List
import Data.Maybe
import Data.Ord
import  qualified Data.Set as DS

-- / interactive play

play :: Board -> Int -> Board
play brd i
 | finished brd = brd
 | isNothing loc  = cleverMove brd
 | otherwise = playl brd (fromJust loc)
 where loc = maybeLocation i

playl :: Board -> Location -> Board
playl brd loc
  | finished brd = brd
  | otherwise = makeSuppliedMove brd loc

-- \ interactive play


-- / strategy
{-
ghci> map (strategyChecker cleverMove) [X,O]
[[(X,0),(O,370),(N,87)],[(X,86),(O,0),(N,6)]]
ghci>
-}

cleverMove :: Board -> Board
cleverMove brd
  | isJust whereToMove = makeMove brd (head $ fromJust whereToMove)
  | otherwise = brd
  where ply = whosMove brd
        opy = otherPlayer ply
        tests :: [(Board -> [Location])]
        tests = [openingMove, canWin ply, canWin opy, canFork ply, blocking, isUnoccupied  centre, oppositeOccupied, isUnoccupied corners, isUnoccupied middles, unplayedLocations]
        whereToMove = find (\mvs -> length mvs > 0)  (map (\f -> f brd) tests)


openingMove :: Board -> [Location]
openingMove brd
  | movesCount brd == 0 = corners

  -- silly special case ... for early in the game when computer plays first
  -- 2nd time computer plays it's too early to "block" if the centre is still open
  {-
  without:
  ghci> map (strategyChecker cleverMove) [X,O]
  [[(X,0),(O,370),(N,87)],[(X,71),(O,2),(N,6)]]
  with:
  ghci> map (strategyChecker cleverMove) [X,O]
  [[(X,0),(O,370),(N,87)],[(X,86),(O,0),(N,6)]]

  -}
  | movesCount brd == 2 && (not $ null $ isUnoccupied centre brd) = centre
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
  | not $ null $ forceableMiddle = forceableMiddle
  | not $ null $ forkableByOpponent = forkableByOpponent
  | not $ null $ forceable = forceable
  | otherwise = []
         -- its a corner & it owns the other corner, so playing here will force oppoent to defend in middle
         --  (thereby keeping opponent from exploiting an opportunity)
         -- looks like maybe here's the flaw ... misses a diagonal corner block - i.e., one row & diag
   where cornerBlock = [l | l <- inboth, (elem l corners) && (length (filter (\sq ->  (tic sq) == ply) (squaresFor brd (adjacentCorners l))) > 0)]
         forceableMiddle = [l | l <- forceable, not $ elem l corners ]
         forkableByOpponent = canFork opy brd
         forceable = canForce ply brd
         inboth = intersect forceable forkableByOpponent
         ply = whosMove brd
         opy = otherPlayer ply

-- unplayed corners occupied by opponent
oppositeOccupied :: Board -> [Location]
oppositeOccupied brd = [loc | loc <- unplayedLocations brd, (elem loc corners)  && ((tic $ squareFor brd $ opposite loc) == oply)]
  where oply = otherPlayer $ whosMove brd

isUnoccupied :: [Location] -> Board -> [Location]
isUnoccupied locs brd  = [loc | (Intersection loc r) <- unplayedIntersections brd, elem loc locs]

canForce :: Player -> Board -> [Location]
canForce ply brd =
  map fst  (filter (\(loc,t) -> t > 0) (map (\(loc,ts) -> (loc, length $ filter (\t -> countForPlayer ply t == 1 && countForPlayer opy t == 0) ts)) (unplayedTallys brd)))
  where opy = otherPlayer ply

-- \ strategy

-- / add'l game play

playUsing :: Strategy -> Board -> Board
playUsing  sty brd
  | finished brd = brd
  | otherwise = sty brd

playARoundUsing :: Strategy -> Board -> Int -> Board
playARoundUsing sty brd i
 | aWinner brd = brd
 | aWinner nextBoard = nextBoard
 | otherwise = sty nextBoard
 where ml = maybeLocation i
       nextBoard = firstMove brd ml
       firstMove brd ml
         | isNothing ml = sty brd
         | otherwise = makeSuppliedMove  brd (fromJust ml)

-- for all of the auto-play strategies accepting a starting board -
-- if the supplied board is in an invalid state, results are unpredictable

-- auto-play from all possible starting positions, return list of winner for each game
autoPlayAllUsing :: Strategy -> [Player]
autoPlayAllUsing strategy = map whoWon $ map (autoPlayFromUsing strategy) definedLocations

-- auto-play a single game, starting with supplied location, using supplied strategy
autoPlayFromUsing :: Strategy -> Location -> Board
autoPlayFromUsing strategy start = autoPlayUsing strategy (makeMove brd start)
  where brd = newBoard
        ply = whosMove brd

-- auto-play a single game, starting with supplied board (which may be partially played), using supplied strategy
autoPlayUsing :: Strategy -> Board -> Board
autoPlayUsing strategy brd
  | aWinner nextBoard = nextBoard
  | not $ hasUnplayed (squares nextBoard) = nextBoard
  | otherwise = autoPlayUsing strategy nextBoard
  where nextBoard = strategy brd

-- auto-play a single game, starting with supplied location & strategy
-- prepend board to list after each move
autoPlayFromUsingTrack :: Strategy -> Location -> [Board]
autoPlayFromUsingTrack strategy start = autoPlayUsingTrack strategy ([makeMove brd start])
  where brd = newBoard
        ply = whosMove brd

-- auto-play a single game, starting with "head" of supplied boards, using supplied strategy
-- prepend board to list after each move
autoPlayUsingTrack :: Strategy -> [Board] -> [Board]
autoPlayUsingTrack strategy [] =  autoPlayUsingTrack strategy [newBoard]
autoPlayUsingTrack strategy bds
  | aWinner nextBoard = nextBoard : bds
  | not $ hasUnplayed (squares nextBoard) = nextBoard : bds
  | otherwise = autoPlayUsingTrack strategy (nextBoard : bds)
  where nextBoard = strategy $ head bds

-- \ add'l game play

