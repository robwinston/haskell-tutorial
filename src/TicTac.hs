module TicTac where

import Data.List
import MyLists

type Square = (Int, Char)

type Board = [Square]

type Intersection = (Int, [[Square]])

-- self-playing game with rudimentary smarts

-- (map aWinner (map autoPlayFrom [1..10]))
-- [False,False,False,False,False,False,False,True,True,True]
-- this shows how failure to pick "best" option is a problem when starting at "numerical" end of board
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
smartMove :: Board -> Board
smartMove board
  -- does current player have a winner? - take it
  | length maybeWinners > 0 && ticCount player (head maybeWinners)  == 2  = move (ticSquare player (pickUnplayedSquare (head maybeWinners))) board
  -- does opponent have a winner? - block it
  | length maybeLosers > 0 && ticCount opponent (head maybeLosers)  == 2  = move (ticSquare player (pickUnplayedSquare (head maybeLosers))) board
  -- find the first-by-rank open square & play it
  | not $ isPhony unplayedSquare = move (ticSquare player unplayedSquare) board
  -- nothing to do
  | otherwise = board
  where
    player = whosMove board
    opponent = otherPlayer player
    maybeWinners = snd (rankTriples board player)
    maybeLosers = snd (rankTriples board opponent)
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
nextMove p b
  | not $ (isUnplayed $ squareFor b p) = b
  | otherwise = move (p, player) b
  where player = whosMove b

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

-- returns unplayed positions by most tics for player in its intersections
rankUnplayedPositions :: Board -> Char -> [Int]
rankUnplayedPositions b p = filter (isUnplayedPosition b) $ map snd $ reverse $ sort $ map (\i -> ((ticCountSum p $ snd $ i), (fst i))) (byIntersections b)


ticCountSum :: Char -> [[Square]] -> Int
ticCountSum player sqls = foldr (+) 0 (map (ticCount player) sqls)

-- organise a board by the interesections for each position
byIntersections :: Board -> [Intersection]
byIntersections  board = map (\p -> (p, winnersFor p board)) [1..9]


-- order triples by how 'good' they are for player Char
rankTriples :: Board -> Char ->  (Char, [[Square]])
rankTriples board player = (player, (sortBy  (rankSqLists player) (playableTriples board)) )

-- by list with most occurences of 'player' ... descending (compare is backwards)
rankSqLists :: Char -> [Square] -> [Square] -> Ordering
rankSqLists player first second
  | ticCount player first > ticCount player second = LT
  | otherwise = GT

-- order squares by "rank" descending
rankSquares :: [Square] -> [Square]
rankSquares squares = sortBy rankSquare squares

-- by "value" of square ... descending (compare is backwards)
-- centre -> a corner -> something else
rankSquare :: Square -> Square -> Ordering
rankSquare sq1 sq2
  | p1 == 5 = LT
  | p2 == 5 = GT
  | elem p1 [1,3,7,9] = LT
  | elem p2 [1,3,7,9] = GT
  | otherwise = GT
  where p1 = fst sq1
        p2 = fst sq2


playableTriples :: Board -> [[Square]]
playableTriples board = filter hasUnplayed (winningTriples board)


-- given a board, return its winning combos
winningTriples :: Board -> [[Square]]
winningTriples board = map (\w -> squaresFor board w) winners
  where  winners = [[1,2,3], [4,5,6], [7,8,9], [1,4,7], [2,5,8], [3,6,9], [1,5,9], [3,5,7]]

-- given a position & board, return postion's winning combos from board
winnersFor :: Int -> Board -> [[Square]]
winnersFor position board = (filter (\w -> elem position (map fst w))) (winningTriples board)

-- square with supplied position
-- this is meant to be the only place where board index == position - 1 matters
squareFor :: Board -> Int -> Square
squareFor b p =  b !! (p-1)

-- squares with supplied positions
squaresFor :: Board -> [Int] -> [Square]
squaresFor b ps =  map (squareFor b) ps

firstSquare :: Board -> [Int] -> Maybe Square
firstSquare b ps
 | length ps == 0 = Nothing
 | otherwise = Just (squareFor b (head ps))


-- from a list of squares, return the 1st unticked one of highest rank
--- will return a phony square if there aren't any, so caller needs to check what's returned
pickUnplayedSquare :: [Square] -> Square
pickUnplayedSquare squares
  | length sqs == 0 = (-1, ' ')
  | otherwise = head $ rankSquares sqs
  where sqs = filter (\sq -> isUnplayed sq) squares

--- given list of postions & list of squares
--- select unticked squares with matching positions & return first one of highest rank
--- will return a phony square if there aren't any, so caller needs to check what's returned
pickUnplayedSquareUsing :: [Int] -> [Square] -> Square
pickUnplayedSquareUsing search squares
  | length subset == 0 = (-1, ' ')
  | otherwise = head $ rankSquares subset
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

isUnplayedPosition :: Board -> Int -> Bool
isUnplayedPosition b p = isUnplayed (squareFor b p)

-- this whole bit of logic & everyone employing it needs refactoring to  use Maybe ...
isPhony :: Square -> Bool
isPhony square = (fst square) < 1 || (fst square) > 9 || not ((elem (snd square) [' ','x','o']))
