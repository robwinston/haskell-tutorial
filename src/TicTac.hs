module TicTac where

import Data.List
import MyLists

type Square = (Int, Char)

type Board = [Square]

-- play game # n, return the board when game ends
-- 362,880 possible game sequences (although there's only 26,830 distinct games)
playGame :: Int -> Board
playGame n =  snd (moveThrough (game, newBoard))
  where game = permu [1..9] !! mod n 362880

-- given a sequence of moves & a board,
-- play until someone wins or no more moves
moveThrough :: ([Int], Board) -> ([Int], Board)
moveThrough (moves, board)
  | length moves == 0 = (moves, board)
  | aWinner board = (moves, board)
  | otherwise = moveThrough (tail moves, (nextMove (head moves) board))

-- given a position, tic for next player
-- ignore if square is occupied
nextMove :: Int -> Board -> Board
nextMove position board
  | snd (board !! (position - 1)) /= ' ' = board
  | otherwise = move (position, player) board
  where player = whosMove board

move :: Square -> Board -> Board
move square board
  | position == 1 = square : rightBoard
  | position == 9 = init board ++ [square]
  | otherwise = init leftBoard ++ [square] ++ rightBoard
  where position = fst square
        sections = splitAt position board
        leftBoard = fst sections
        rightBoard = snd sections

moves :: Board -> [Square]
moves b = filter (\sq -> snd sq == ' ') b

winner :: Char -> Board -> Bool
winner c b =  or $ map (\w -> isInfixOf w ticked) winners
  where ticked = map fst (filter (\sq -> snd sq == c) b)
        winners = [[1,2,3], [4,5,6], [7,8,9], [1,5,9], [3,5,7]]

aWinner :: Board -> Bool
aWinner board = fst (whoWon board) /= ' '

whoWon :: Board -> (Char, Int)
whoWon b
 | winner 'x' b = ('x', m)
 | winner 'o' b = ('o', m)
 | otherwise = (' ', m)
 where m = 9 - length (moves b)

howWon :: Board -> [Square]
howWon board = [sq | sq <- board, snd sq == fst (whoWon board)]

-- assumes 'x' plays first ...
whosMove :: Board -> Char
whosMove b
 | movesLeft == 0 = ' '
 | mod movesLeft 2 == 0 = 'o'
 | otherwise = 'x'
 where movesLeft = length (moves b)

newBoard :: Board
newBoard = map (\i -> (i, ' ')) [1..9]




