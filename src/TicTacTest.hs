module TicTac.TicTacTest where

import TicTac
import TicTacCore

import Data.List
import Data.Maybe
import Data.Ord
import  qualified Data.Set as DS
import  qualified Data.Map as DM


strategyChecker :: Strategy -> Player -> [(Player, Int)]
strategyChecker sty ply =  map (\py -> (py,(winnersFor outcomes py))) [X,O,N]
  where allGames = allPossibleGames sty ply newBoard
        outcomes = map gameOutcome allGames

-- Given a strategy, which player is human, and a board, return all possible outcomes human v computer using supplied strategy
allPossibleGames :: Strategy -> Player -> Board -> [Game]
allPossibleGames sty ply brd = map (aGameFrom) (playAllPossibleRounds sty ply [[brd]])


playAllPossibleRounds :: Strategy -> Player -> [[Board]] -> [[Board]]
playAllPossibleRounds sty  ply [] = playAllPossibleRounds sty ply [[newBoard]]
playAllPossibleRounds sty ply bdss
  | null $ filter (\bds -> not $ finished $ head bds) bdss = bdss
  | otherwise = playAllPossibleRounds sty ply (concat $ map (\bds -> playPossibleRounds sty ply bds) bdss)

-- for the head of a given board sequence, prepend all of the next possible rounds
-- where a round is
--  1) a specified move - representing a "human"
--  2) the computer's response (using specified strategy)
-- Player param says which player is the human ...
-- a given play is short-cicuited when a winner/draw is reached
playPossibleRounds :: Strategy -> Player -> [Board] -> [[Board]]
playPossibleRounds sty ply bseq
  | finished $ head bseq = [bseq]
  | plyToPlay == N = [bseq]
  | plyToPlay == ply = map (autoNextMove sty) bseqn
  | otherwise = concat $ map playPossibles bseqn
  where plyToPlay = whosMove $ head bseq
        bseqn
          | plyToPlay == ply = playPossibles bseq
          | otherwise = [autoNextMove sty bseq]

-- for the head of a given board sequence, prepend all of the n possible moves, yielding n board sequences
playPossibles :: [Board] -> [[Board]]
playPossibles (brd:bs)
  | finished brd = [brd:bs]
  | otherwise = map (\ba -> ba:(brd:bs)) nextMoves
  where upl = map location $ filter isUnplayed $ squares brd
        nextMoves = map (makeSuppliedMove brd) upl

-- for a list of boards, prepend next move using strategy
autoNextMove :: Strategy -> [Board] -> [Board]
autoNextMove  _ [] = []
autoNextMove sty (brd:bs)
  | finished brd =  (brd:bs)
  | otherwise = sty brd : (brd:bs)


-- interactive play
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

-- \ game play


-- / programmed play ... useful for testing

-- play a supplied sequence of moves, alternating players
playMoves :: [Location] -> Board
playMoves ps =  snd $ moveThrough (ps, newBoard)

playAllGamesFrom :: Int -> [Board]
playAllGamesFrom idx =
 playGame idx : playAllGamesFrom (idx+1)

-- play pre-defined game # n, return the board when game ends
-- 362,880 possible game sequences (although there's only 26,830 distinct games)
playGame :: Int -> Board
playGame n =  snd $ moveThrough (game, newBoard)
  where game =  allPlaySequences !! mod n 362880

allPlaySequences = permu definedLocations

-- given a sequence of locations & a board,
-- play until someone wins or no more moves
moveThrough :: ([Location], Board) -> ([Location], Board)
moveThrough (ls, brd)
  | length ls == 0 = (ls, brd)
  | aWinner brd = (ls, brd)
  | otherwise = moveThrough (tail ls, (makeSuppliedMove brd (head ls)))

-- \ programmed play

-- load some test data ... for ghci devel
apg = allPossibleGames cleverMove O newBoard
apo = map gameOutcome apg
apgo = zip apg apo

apgX = gamesFor apg X
apgO = gamesFor apg O
apgN = gamesFor apg N

apgoO = [g | (g, (Outcome ply m)) <- apgo, ply == O]
apgoX = [g | (g, (Outcome ply m)) <- apgo, ply == X]
apgoN = [g | (g, (Outcome ply m)) <- apgo, ply == N]

gO0 = head apgoO
gX0 = head apgoX
gN0 = head apgoN

gO = head apgO
brd = fromJust $ boardForMove gO 1
ply = whosMove brd
opy = otherPlayer ply
forceable = canForce ply brd
forkableByOpponent = canFork opy brd
inboth = intersect forceable forkableByOpponent
forceableOnly = diffs forceable forkableByOpponent
blockables = blocking brd
-- its a corner & it owns the other corner, so playing here will force oppoent to defend in middle
--  (thereby keeping opponent from exploiting an opportunity)
cornerBlock = [l | l <- inboth, (elem l corners) && (length (filter (\sq ->  (tic sq) == ply) (squaresFor brd (adjacentCorners l))) > 0)]
-- these are really forceableMiddle, i.e. middleBlock ?
forceToMiddle = [l | l <- forceable, (not $ elem l corners) ]



