module TicTac.Test where

import TicTac.Play
import TicTac.Simple
import TicTac.Common

import Data.List
import Data.Maybe
import Data.Ord
import  qualified Data.Set as DS
import  qualified Data.Map as DM

{-
25/04/16 54d5235
ghci> map (strategyChecker cleverMove) [X,O]
[[(X,0),(O,370),(N,87)],[(X,86),(O,0),(N,6)]]
ghci> map (strategyChecker smarterMove) [X,O]
[[(X,18),(O,346),(N,261)],[(X,132),(O,0),(N,8)]]
ghci> map (strategyChecker smartMove) [X,O]
[[(X,94),(O,376),(N,211)],[(X,68),(O,6),(N,8)]]
ghci>
-}

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

-- test data retrieval ... for ghci devel
apg = allPossibleGames cleverMove O newBoard
apo = map gameOutcome apg
apgo = zip apg apo

apgX = gamesFor apg X
apgO = gamesFor apg O
apgN = gamesFor apg N

