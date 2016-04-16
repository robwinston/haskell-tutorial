module TicTac where

import Data.List
import Data.Maybe
import MyLists

type Player = Char
type Position = Int
type Square = (Position, Player)
type Board = [Square]
type Intersection = (Position, [[Square]])

-- self-playing game with rudimentary smarts

autoPlayFrom :: Position -> Board
autoPlayFrom start = autoPlay (move (start, player) board)
  where board = newBoard
        player = whosMove board

autoPlay :: Board -> Board
autoPlay board = autoPlayUsing smarterMove board

{-
-- failure to pick "best" option is a problem when starting at "numerical" end of board
ghci> map aWinner $ map (autoPlayFromUsing smartMove) [1..9]
[False,False,False,False,False,False,False,True,True]

-- hmm, attempt at improvement just moved the problem
ghci> map aWinner $ map (autoPlayFromUsing smarterMove) [1..9]
[False,False,True,False,False,True,False,False,False]

-}

autoPlayFromUsing :: (Board -> Board) -> Position -> Board
autoPlayFromUsing strategy start = autoPlayUsing strategy (move (start, player) board)
  where board = newBoard
        player = whosMove board

autoPlayUsing :: (Board -> Board) -> Board -> Board
autoPlayUsing strategy board
  | aWinner nextBoard = nextBoard
  | not $ hasUnplayed nextBoard = nextBoard
  | otherwise = autoPlayUsing strategy nextBoard
  where nextBoard = strategy board


-- given a board, try to make best move
-- streamlined strategy ... this will autoplay every starting position to a draw
-- remains to be seen if a human can outsmart it  ...
smarterMove :: Board -> Board
smarterMove board
  | isJust unplayedSquare = move (ticSquare player (fromJust unplayedSquare)) board
  | otherwise = board
  where  player = whosMove board
         opponent = otherPlayer player
         possibleWinners = snd (rankTriples board player)
         possibleLosers = snd (rankTriples board opponent)
         unplayedSquare = bestUnplayedSquare board player


-- given a board, try to make best move
-- initial simplistic logic ....
smartMove :: Board -> Board
smartMove board
  -- does current player have a winner? - take it
  | length possibleWinners > 0 && ticCount player (head possibleWinners)  == 2  = moveMaybe (ticSquareMaybe player (pickUnplayedSquare (head possibleWinners))) board
  -- does opponent have a winner? - block it
  | length possibleLosers > 0 && ticCount opponent (head possibleLosers)  == 2  = moveMaybe (ticSquareMaybe player (pickUnplayedSquare (head possibleLosers))) board
  -- find the first-by-rank open square & play it
  | isJust unplayedSquare = move (ticSquare player (fromJust unplayedSquare)) board
  -- nothing to do
  | otherwise = board
  where
    player = whosMove board
    opponent = otherPlayer player
    possibleWinners = snd (rankTriples board player)
    possibleLosers = snd (rankTriples board opponent)
    unplayedSquare = pickUnplayedSquare board


-- play pre-defined game # n, return the board when game ends
-- 362,880 possible game sequences (although there's only 26,830 distinct games)
playGame :: Int -> Board
playGame n =  snd (moveThrough (game, newBoard))
  where game = permu [1..9] !! mod n 362880

-- given a sequence of moves & a board,
-- play until someone wins or no more moves
moveThrough :: ([Position], Board) -> ([Position], Board)
moveThrough (moves, board)
  | length moves == 0 = (moves, board)
  | aWinner board = (moves, board)
  | otherwise = moveThrough (tail moves, (nextMove (head moves) board))

-- given a position, tic for next player
-- ignore if square is occupied
nextMove :: Position -> Board -> Board
nextMove p b
  | not $ (isUnplayed $ squareFor b p) = b
  | otherwise = move (p, player) b
  where player = whosMove b

moveMaybe :: Maybe Square -> Board -> Board
moveMaybe msq board
    | isNothing msq = board
    | otherwise = move (fromJust msq) board

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

winner :: Player -> Board -> Bool
winner c b =  or $ map (\w -> isInfixOf w ticked) winners
  where ticked = map fst (filter (\sq -> snd sq == c) b)
        winners = [[1,2,3], [4,5,6], [7,8,9], [1,5,9], [3,5,7]]

theWinner :: Board -> Player
theWinner board
  | winner /= ' ' = winner
  | otherwise = '/'
  where winner = fst $ whoWon board

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
whosMove :: Board -> Player
whosMove b
 | movesLeft == 0 = ' '
 | mod movesLeft 2 == 0 = 'o'
 | otherwise = 'x'
 where movesLeft = length (moves b)

newBoard :: Board
newBoard = map (\i -> (i, ' ')) [1..9]


bestUnplayedSquare :: Board -> Player -> Maybe Square
bestUnplayedSquare b p
 | length possiblePositions == 0 = Nothing
 | otherwise = Just (squareFor b (head possiblePositions))
 where possiblePositions = rankUnplayedPositions b p

-- returns unplayed positions ranked by most tics for player in its intersections
rankUnplayedPositions :: Board -> Player -> [Position]
rankUnplayedPositions b p =
 map fst (sortBy (rankIntersectionFor p) (byIntersectionsUnplayed b))
  -- map snd $ reverse $ sort $ map (\i -> ((ticCountSumUseful p $ snd $ i), (fst i))) (byIntersectionsUnplayed b)

-- if one intersection has more player ticks, it's better
-- if they're the same, rank by their position
-- ... all descending, which is why they look backwards
-- avoids (albeit minor in this case) reverse expense ... because I never want them ascending
rankIntersectionFor :: Player -> Intersection -> Intersection -> Ordering
rankIntersectionFor p i1 i2
  | i1Score > i2Score = LT
  | i1Score < i2Score = GT
  | i1Score == i2Score = rankPosition (fst i1) (fst i2)
  where i1Score = ticCountSumUseful p (snd i1)
        i2Score = ticCountSumUseful p (snd i2)

byIntersectionsUnplayed :: Board -> [Intersection]
byIntersectionsUnplayed b =  filter (\i -> isUnplayedPosition b (fst i)) (byIntersections b)

-- organise a board by the interesections for each position
byIntersections :: Board -> [Intersection]
byIntersections  board = map (\p -> (p, winnersFor p board)) [1..9]


-- order triples by how 'good' they are for player Char
rankTriples :: Board -> Player ->  (Player, [[Square]])
rankTriples board player = (player, (sortBy  (rankSqLists player) (playableTriples board)) )

-- by list with most occurences of 'player' ... descending (compare is backwards)
rankSqLists :: Player -> [Square] -> [Square] -> Ordering
rankSqLists player first second
  | ticCount player first > ticCount player second = LT
  | ticCount player first < ticCount player second = GT
  | otherwise = EQ

-- order squares by "rank" descending
rankSquares :: [Square] -> [Square]
rankSquares squares = sortBy rankSquare squares

rankSquare :: Square -> Square -> Ordering
rankSquare sq1 sq2 = rankPosition (fst sq1) (fst sq2)

-- by "value" of a position ... descending (compare is backwards)
-- centre -> a corner -> something else
rankPosition :: Position -> Position -> Ordering
rankPosition p1 p2
  | p1 == 5 = LT
  | p2 == 5 = GT
  | elem p1 [1,3,7,9] = LT
  | elem p2 [1,3,7,9] = GT
  | otherwise = EQ

playableTriples :: Board -> [[Square]]
playableTriples board = filter hasUnplayed (winningTriples board)


-- given a board, return its winning combos
winningTriples :: Board -> [[Square]]
winningTriples board = map (\w -> squaresFor board w) winners
  where  winners = [[1,2,3], [4,5,6], [7,8,9], [1,4,7], [2,5,8], [3,6,9], [1,5,9], [3,5,7]]

-- given a position & board, return postion's winning combos from board
winnersFor :: Position -> Board -> [[Square]]
winnersFor position board = (filter (\w -> elem position (map fst w))) (winningTriples board)

-- square with supplied position
-- this is meant to be the only place where board index == position - 1 matters
squareFor :: Board -> Position -> Square
squareFor b p =  b !! (p-1)

-- squares with supplied positions
squaresFor :: Board -> [Position] -> [Square]
squaresFor b ps =  map (squareFor b) ps

firstSquare :: Board -> [Position] -> Maybe Square
firstSquare b ps
 | length ps == 0 = Nothing
 | otherwise = Just (squareFor b (head ps))


-- from a list of squares, return the 1st unticked one of highest rank (or Nothing)
pickUnplayedSquare :: [Square] -> Maybe Square
pickUnplayedSquare squares
  | length sqs == 0 = Nothing
  | otherwise = Just (head $ rankSquares sqs)
  where sqs = filter (\sq -> isUnplayed sq) squares

--- given list of postions & list of squares
--- select unticked squares with matching positions & return first one of highest rank (or Nothing)
pickUnplayedSquareUsing :: [Int] -> [Square] -> Maybe Square
pickUnplayedSquareUsing search squares
  | length subset == 0 = Nothing
  | otherwise = Just (head $ rankSquares subset)
  where subset = filter (\sq -> (elem (fst sq) search) && isUnplayed sq) squares

-- weights a collection of "rows", by summing player's tics for those rows not occuped by opponent
ticCountSumUseful :: Player -> [[Square]] -> Int
ticCountSumUseful player sqls = foldr (+) 0 (map (ticCount player) (filter (isUnplayedBy (otherPlayer player)) sqls))

-- weights a collection of "rows", by summing player's tics
ticCountSum :: Player -> [[Square]] -> Int
ticCountSum player sqls = foldr (+) 0 (map (ticCount player) sqls)

ticCount :: Player -> [Square] -> Int
ticCount player squares = length $ filter (\a -> snd a == player) squares

ticSquareMaybe :: Player -> Maybe Square -> Maybe Square
ticSquareMaybe p msq
  | isNothing msq = msq
  | otherwise = Just (ticSquare p square)
  where square = fromJust msq

ticSquare :: Player -> Square -> Square
ticSquare char square = (fst square, char)

otherPlayer :: Player -> Player
otherPlayer p
  | p == 'o' = 'x'
  | p == 'x' = 'o'
  | otherwise = ' '


isUnplayedBy :: Player -> [Square] -> Bool
isUnplayedBy p squares = length (filter (\sq -> snd sq == p) squares) == 0

hasUnplayed :: [Square] -> Bool
hasUnplayed squares = length (filter (\sq -> snd sq == ' ') squares) > 0

isUnplayed :: Square -> Bool
isUnplayed square = snd square == ' '

isUnplayedPosition :: Board -> Position -> Bool
isUnplayedPosition b p = isUnplayed (squareFor b p)

