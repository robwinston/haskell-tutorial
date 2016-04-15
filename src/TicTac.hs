module TicTac where

import Data.List
import MyLists

type Square = (Int, Char)

type Board = [Square]

-- self-playing game with rudimentary smarts

-- not every autoPlay game's a draw ... so work to be done
-- or (map aWinner (map autoPlayFrom [1..9]))  == True
autoPlayFrom :: Int -> Board
autoPlayFrom start = autoPlay (move (start, player) board)
  where board = newBoard
        player = whosMove board

autoPlay :: Board -> Board
autoPlay board
  | aWinner nextBoard = nextBoard
  | not $ hasUnplayed nextBoard = nextBoard
  | otherwise = autoPlay nextBoard
  where nextBoard = smartMove board



-- given a board, try to make best move
-- initial simplistic logic ....
-- does current player have a winner? - take it
-- does opponent have a winner? - block it
-- is the centre free? - take it <- this isn't always better than a corner
-- is a corner free? - take it   <- but doesn't pick the best corner
-- find the first open square & play it <- but doesn't pick the best space left
smartMove :: Board -> Board
smartMove board
  | length maybeWinners > 0 && ticCount player (head maybeWinners)  == 2  = move (ticSquare player (pickUnplayedSquare (head maybeWinners))) board
  | length maybeLosers > 0 && ticCount opponent (head maybeLosers)  == 2  = move (ticSquare player (pickUnplayedSquare (head maybeLosers))) board
  -- posotions are sequenced from one,
  -- makes it slightly easier to visualise elsewhere, but also a bit confusing in places like this
  | isUnplayed (board !! 4) = move (5, player) board
  | not $ isPhony unplayedCorner = move (ticSquare player unplayedCorner) board
  | not $ isPhony unplayedSquare = move (ticSquare player unplayedSquare) board
  | otherwise = board
  where
    player = whosMove board
    opponent = otherPlayer player
    maybeWinners = snd (rankTriples board player)
    maybeLosers = snd (rankTriples board opponent)
    -- this really should pick the 'best' unplayed corner
    unplayedCorner = pickUnplayedSquareUsing [1,3,7,9] board
    -- ditto
    unplayedSquare = pickUnplayedSquare board


-- play pre-defined game # n, return the board when game ends
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

moves :: [Square] -> [Square]
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

-- give all moves for winner, not just winning sequence
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



-- order triples by how 'good' they are for player Char
rankTriples :: Board -> Char ->  (Char, [[Square]])
rankTriples board player = (player, (sortBy  (rankSqLists player) (playableTriples board)) )

-- by list with most occurences of 'player' ... descending
rankSqLists :: Char -> [Square] -> [Square] -> Ordering
rankSqLists player first second
  | ticCount player first > ticCount player second = LT
  | otherwise = GT

playableTriples :: Board -> [[Square]]
playableTriples board = filter hasUnplayed (winningTriples board)

winningTriples :: Board -> [[Square]]
winningTriples board = map (\w -> extractSquares board w) winners
  where  winners = [[1,2,3], [4,5,6], [7,8,9], [1,4,7], [2,5,8], [3,6,9], [1,5,9], [3,5,7]]

-- extract squares with supplied positions
extractSquares :: Board -> [Int] -> [Square]
extractSquares board sqs =  map (\n -> board !! (n-1)) sqs

-- from a list of squares, return the 1st unticked one
pickUnplayedSquare :: [Square] -> Square
pickUnplayedSquare squares
  | length sqs == 0 = (-1, ' ')
  | otherwise = head sqs
  where sqs = filter (\sq -> isUnplayed sq) squares

--- given a list of postions & squares
--- select unticked ones with matching positions & return first one
--- will return a phony square if there aren't any, so need to check this if relevant
--- and it doesn't prioritise the list of positions (but it really should)
pickUnplayedSquareUsing :: [Int] -> [Square] -> Square
pickUnplayedSquareUsing search squares
  | length subset == 0 = (-1, ' ')
  | otherwise = head subset
  where subset = filter (\sq -> (elem (fst sq) search) && isUnplayed sq) squares


ticCount :: Char -> [Square] -> Int
ticCount player squares = length $ filter (\a -> snd a == player) squares

ticSquare :: Char -> Square -> Square
ticSquare char square = (fst square, char)

otherPlayer :: Char -> Char
otherPlayer p
  | p == 'o' = 'x'
  | p == 'x' = 'o'
  | otherwise = ' '

hasUnplayed :: [Square] -> Bool
hasUnplayed squares = length (filter (\sq -> snd sq == ' ') squares) > 0

isUnplayed :: Square -> Bool
isUnplayed square = snd square == ' '

isPhony :: Square -> Bool
isPhony square = (fst square) < 1 || (fst square) > 9 || not ((elem (snd square) [' ','x','o']))
