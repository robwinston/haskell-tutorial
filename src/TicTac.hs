module TicTac where

import Data.List
import Data.Maybe
import MyLists

type Player = Char
type Position = Int
type Square = (Position, Player)
type Board = [Square]
type Intersection = (Position, [[Square]])


{-
both strategies play to draw against self,
but I think "smarterMove" can better hold its own against a human being
... will test when add IO to mix
ghci> playAllUsingWinners smarterMove
"/////////"  <- '/' means it's a draw (theWinner function does this)
ghci> playAllUsingWinners smartMove
"/////////"
ghci>
-}

playAllUsingWinners :: (Board -> Board) -> String
playAllUsingWinners strategy = map theWinner $ map (autoPlayFromUsing strategy) [1..9]

autoPlayFrom :: Position -> Board
autoPlayFrom start = autoPlay (nextMove start board)
  where board = newBoard

autoPlay :: Board -> Board
autoPlay board = autoPlayUsing smarterMove board


-- autoPlay a game, but pick starting move
-- keep track of play as it proceeds
autoPlayFromUsingTrack :: (Board -> Board) -> Position -> [Board]
autoPlayFromUsingTrack strategy start = autoPlayUsingTrack strategy ([move (start, player) board])
  where board = newBoard
        player = whosMove board

-- autoPlay a game, using first of supplied boards which may or may not have moves already made
-- keep adding boards to list as game play proceeds
autoPlayUsingTrack :: (Board -> Board) -> [Board] -> [Board]
autoPlayUsingTrack strategy boards
  | aWinner nextBoard = nextBoard : boards
  | not $ hasUnplayed nextBoard = nextBoard : boards
  | otherwise = autoPlayUsingTrack strategy (nextBoard : boards)
  where nextBoard = strategy $ head boards

-- autoPlay a game employing supplied strategy, but pick starting move
autoPlayFromUsing :: (Board -> Board) -> Position -> Board
autoPlayFromUsing strategy start = autoPlayUsing strategy (move (start, player) board)
  where board = newBoard
        player = whosMove board

-- autoPlay a game employing supplied strategy, using supplied board which may or may not have moves already made
autoPlayUsing :: (Board -> Board) -> Board -> Board
autoPlayUsing strategy board
  | aWinner nextBoard = nextBoard
  | not $ hasUnplayed nextBoard = nextBoard
  | otherwise = autoPlayUsing strategy nextBoard
  where nextBoard = strategy board


-- / game strategies

-- given a board, try to make best next move
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

-- from a list of squares, return the 1st unticked one of highest rank (or Nothing)
-- using fairly simple ranking
pickUnplayedSquare :: [Square] -> Maybe Square
pickUnplayedSquare squares
  | length sqs == 0 = Nothing
  | otherwise = Just (head $ rankSquares sqs)
  where sqs = filter (\sq -> isUnplayed sq) squares

-- from a list of squares, return the 1st unticked one of highest rank (or Nothing)
-- using more involved ranking
bestUnplayedSquare :: Board -> Player -> Maybe Square
bestUnplayedSquare b p
 | length possiblePositions == 0 = Nothing
 | otherwise = Just (squareFor b (head possiblePositions))
 where possiblePositions = rankUnplayedPositions b p

-- returns unplayed positions ranked by most tics for player in its intersections
rankUnplayedPositions :: Board -> Player -> [Position]
rankUnplayedPositions b p =
 map fst (sortBy (rankIntersectionFor p) (byIntersectionsUnplayed b))

-- if one intersection has a better score, it's better
-- if they're the same, rank by their position
-- ... all descending, which is why they look backwards
-- avoids (albeit minor in this case) reverse expense ... because I never want them ascending
rankIntersectionFor :: Player -> Intersection -> Intersection -> Ordering
rankIntersectionFor p i1 i2
  | i1Score > i2Score = LT
  | i1Score < i2Score = GT
  | i1Score == i2Score = rankPosition (fst i1) (fst i2)
  where i1Score = scoreIntersectionFor p i1
        i2Score = scoreIntersectionFor p i2


-- this is the meat of the "smarter" strategy
scoreIntersectionFor :: Player -> Intersection -> Int
scoreIntersectionFor p i
 -- has an about to win
 | length (filter (\r -> ticCount p r == 2) rows) > 0 = 12
 -- has an about to lose
 | length (filter (\r -> ticCount o r == 2) rows) > 0 = 11
 -- it's an open corner & opponent occupies opposite corner
 -- (if opponent allowed to tick, creates two magic corners, making a block impossible)
 | elem position [1,3,7,9] && snd (squareAt opposition rows) == otherPlayer p = 10
 -- is a "magic corner", i.e. has two rows with tics
 | length (filter (\r -> ticCount p r == 1) rows) > 1 = 9
 -- is an opponent's "magic corner"
 | length (filter (\r -> ticCount o r == 1) rows) > 1 = 8
 -- is centre or corner with "connected" tics for opponent
 | elem position [1,3,5,7,9] && length (filter (\r -> ticCount o r == 1) rows) > 0 = 7
 -- is centre or corner with "connected" tics for self
 | elem position [1,3,5,7,9] && length (filter (\r -> ticCount p r == 1) rows) > 0 = 6
 -- is the centre
 | position == 5 = 5
 -- is a corner
 | elem position [1,3,7,9] = 4
 -- otherwise, sum unblocked ticks, at this point,
 -- intutively suspect this will always be 2 or less (if reached), but haven't proved it
 | otherwise = ticCountSumUseful p rows
 where rows = snd i
       position = fst i
       opposition = 10 - position  -- "opposite position"
       o = otherPlayer p
       -- index a square out of an intersection's "triples"
       -- caller knows it will be there, so doesn't protect against empty list
       squareAt :: Position -> [[Square]] -> Square
       squareAt  p lsqs = head $ filter (\i -> fst i == p) sqs
         where sqs = concat lsqs


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


-- \ game strategies


-- / square state functions

-- weights a collection of "rows", by summing player's tics for those rows not occuped by opponent
ticCountSumUseful :: Player -> [[Square]] -> Int
ticCountSumUseful player sqls = foldr (+) 0 (map (ticCount player) (filter (isUnplayedFor (otherPlayer player)) sqls))

ticCount :: Player -> [Square] -> Int
ticCount player squares = length $ filter (\a -> snd a == player) squares

ticSquareMaybe :: Player -> Maybe Square -> Maybe Square
ticSquareMaybe p msq
  | isNothing msq = msq
  | otherwise = Just (ticSquare p square)
  where square = fromJust msq

ticSquare :: Player -> Square -> Square
ticSquare char square = (fst square, char)

-- squares with supplied positions
squaresFor :: Board -> [Position] -> [Square]
squaresFor b ps =  map (squareFor b) ps

-- square with supplied position
-- this is meant to be the only place where board index == position - 1 matters
squareFor :: Board -> Position -> Square
squareFor b p =  b !! (p-1)

isUnplayedFor :: Player -> [Square] -> Bool
isUnplayedFor p squares = length (filter (\sq -> snd sq == p) squares) == 0

hasUnplayed :: [Square] -> Bool
hasUnplayed squares = length (filter (\sq -> snd sq == ' ') squares) > 0

isUnplayedPosition :: Board -> Position -> Bool
isUnplayedPosition b p = isUnplayed (squareFor b p)

isUnplayed :: Square -> Bool
isUnplayed square = snd square == ' '

-- \ square state functions


-- / board state functions

newBoard :: Board
newBoard = map (\i -> (i, ' ')) [1..9]

playableTriples :: Board -> [[Square]]
playableTriples board = filter hasUnplayed (winningTriples board)

-- given a board, return its winning combos
winningTriples :: Board -> [[Square]]
winningTriples board = map (\w -> squaresFor board w) winners
  where  winners = [[1,2,3], [4,5,6], [7,8,9], [1,4,7], [2,5,8], [3,6,9], [1,5,9], [3,5,7]]

-- given a position & board, return postion's winning combos from board
winnersFor :: Position -> Board -> [[Square]]
winnersFor position board = (filter (\w -> elem position (map fst w))) (winningTriples board)

byIntersectionsUnplayed :: Board -> [Intersection]
byIntersectionsUnplayed b =  filter (\i -> isUnplayedPosition b (fst i)) (byIntersections b)

-- represent a board as a list of intersections for each position
byIntersections :: Board -> [Intersection]
byIntersections  board = map (\p -> (p, winnersFor p board)) [1..9]

winner :: Board -> Player -> Bool
winner b p =  or $ map (\w -> isInfixOf w ticked) winners
  where ticked = map fst (filter (\sq -> snd sq == p) b)
        winners = [[1,2,3], [4,5,6], [7,8,9], [1,5,9], [3,5,7]]

theWinner :: Board -> Player
theWinner board = fst $ whoWonMoves board

aWinner :: Board -> Bool
aWinner board = theWinner board /= '/'

-- who won & how many moves it took
--  ('/',9) == a draw
--  ('/', [0..8]) == an unfinished game
whoWonMoves :: Board -> (Char, Int)
whoWonMoves b
 | winner b 'x' = ('x', m)
 | winner b 'o' = ('o', m)
 | otherwise = ('/', m)
 where m = 9 - length (unplayedSquares b)

-- give all moves made by winning player, not just winning sequence
howWon :: Board -> (Player, [Position])
howWon board = (winner, map fst squares)
  where winner = theWinner board
        squares = [sq | sq <- board, snd sq == winner]

-- if board is empty, assumes 'x' plays first ...
whosMove :: Board -> Player
whosMove b
 | movesLeft == 0 = ' '
 | mod movesLeft 2 == 0 = 'o'
 | otherwise = 'x'
 where movesLeft = length (unplayedSquares b)

unplayedSquares :: [Square] -> [Square]
unplayedSquares b = filter (\sq -> snd sq == ' ') b


otherPlayer :: Player -> Player
otherPlayer p
  | p == 'o' = 'x'
  | p == 'x' = 'o'
  | otherwise = ' '

-- \ board state functions

-- / mechanics

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

-- \ mechanics

-- / programmed play ... useful for testing

-- play a supplied sequence of moves, alternating players
playMoves :: [Int] -> Board
playMoves ps =  snd (moveThrough (ps, newBoard))

-- play pre-defined game # n, return the board when game ends
-- 362,880 possible game sequences (although there's only 26,830 distinct games)
playGame :: Int -> Board
playGame n =  snd (moveThrough (game, newBoard))
  where game = permu [1..9] !! mod n 362880

-- given a sequence of moves & a board,
-- play until someone wins or no more moves
moveThrough :: ([Position], Board) -> ([Position], Board)
moveThrough (unplayedSquares, board)
  | length unplayedSquares == 0 = (unplayedSquares, board)
  | aWinner board = (unplayedSquares, board)
  | otherwise = moveThrough (tail unplayedSquares, (nextMove (head unplayedSquares) board))

-- \ programmed play

