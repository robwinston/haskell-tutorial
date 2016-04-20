module TicTac where

import Data.List
import Data.Maybe
import Data.Set
import MyLists

data Player = N | X | O
  deriving (Eq, Ord, Show, Bounded, Enum)


data Score =  Unplayable | Blocked | Playable | MaybeOther | MaybeMe | ForkableOther | ForkableMe | Loser | Winner
  deriving (Eq, Ord, Show, Bounded, Enum)

data Rank = Other | Corner | Centre
  deriving (Eq, Ord, Show, Bounded, Enum)


type Move = Position

type Position = Int
opposition :: Position -> Position
opposition p = 10 - p
itsRank :: Position -> Rank
itsRank p
 | aCorner p = Corner
 | theCentre p = Centre
 | otherwise = Other
rankPosition :: Position -> Position -> Ordering
rankPosition p1 p2 = compare (itsRank p1) (itsRank p2)    -- reversed for descending

winners :: [[Position]]
winners = [[1,2,3], [4,5,6], [7,8,9], [1,4,7], [2,5,8], [3,6,9], [1,5,9], [3,5,7]]

theCentre p = p == centre
aCorner p = elem p corners

corners :: [Position]
corners = [1,3,7,9]

centre :: Position
centre = 5


-- / Square
data Square = Square {
                 location :: Position,
                 tic :: Player,
                 move :: Move }
              deriving (Eq, Ord)

instance Show Square
  where show square@(Square l p m) = showSquare (square)
showSquare :: Square -> String
showSquare (Square {location = l, tic = p, move = m}) = "|" ++ show l ++ ":" ++ show p ++ ":" ++ show m ++ "|"
-- \ Square

-- / Board
data Board = Board  {
               squares :: [Square] }
             deriving (Eq)

newBoard :: Board
newBoard = Board (Data.List.map (\i -> Square i N 0) [1..9])

oppositeSq :: Board -> Square -> Square
oppositeSq board (Square p _ _)  = squareFor board p


instance Show Board
  where show (Board sqs)  = showBoard (Board sqs)
showBoard :: Board -> String
showBoard b@(Board {squares = sqs} ) =  (fst (squaresToGrid ("", sqs))) ++ (boardState b)
  where boardState :: Board -> String
        boardState b
          | aWinner b = (show whoWon)  ++ " wins!\n"
          | nextPlayer == N = "It's a draw\n"
          | otherwise = (show nextPlayer) ++ " to move\n"
          where whoWon = theWinner b
                nextPlayer = whosMove b


squaresToGrid :: (String , [Square]) -> (String, [Square])
squaresToGrid (gridString, squares)
  | length squares == 0 = (gridString, squares)
  | otherwise =  squaresToGrid ((gridString ++ (removeDupes (concat (Data.List.map justTic row))) ++ "\n"), whatsLeft)
  where (row, whatsLeft) = splitAt 3 squares
        justTic :: Square -> String
        justTic square  = "|" ++ show (tic square) ++ "|"

-- \ Board

-- / Game
data Game = Game {
         boards :: [Board]
       }
       deriving (Eq)

instance Show Game
  where show (Game bds) = showGame (Game bds)
showGame :: Game -> String
showGame (Game {boards = bds}) = gameString "Game sequence: \n\n" bds


gameString :: String -> [Board] -> String
gameString s [] = s ++ "No boards!"
gameString s (b:bds)
  | length bds == 0 = s ++ show b ++ "\n" ++ show (movesMade b) ++ " moves made\n"
  | otherwise = gameString (s ++ show b ++ "\n") bds

-- \ Game

type Strategy = (Board -> Board)
type Scorer = (Player -> Intersection -> Int)

data Intersection = Intersection  {
                      nexus :: Position,
                      rows :: [[Square]] }
                    deriving (Eq, Show)

-- / game play

play :: Board -> Position -> Board
play board position = playUsing smarterMove board position

playUsing :: Strategy -> Board -> Position -> Board
playUsing  strategy board position
  | aWinner board = board
  | position < 1 || position > 9 = strategy board
  | otherwise = makeThisMove position board

playARound :: Board -> Position -> Board
playARound board position = playARoundUsing smarterMove board position

playARoundUsing  :: Strategy -> Board -> Position -> Board
playARoundUsing strategy board position
  | aWinner board = board
  | aWinner nextBoard = nextBoard
  | otherwise = strategy nextBoard
  where nextBoard = firstMove board position
        firstMove b p
          | p < 1 || p > 9 = strategy b
          | otherwise = makeThisMove p b

{-
ghci> autoPlayAllUsing smartMove
[N,N,N,N,N,N,N,X,X]  <- X wins when starting with position 8 or 9
ghci> autoPlayAllUsing smarterMove
[N,N,N,N,N,N,N,N,N] <- plays to draw in all cases
ghci>
-}

-- for all of the auto-play strategies accepting a starting board -
-- if this board is in an invalid state, results are unpredictable

-- auto-play from all possible starting positions, return list of winner for each game
autoPlayAllUsing :: Strategy -> [Player]
autoPlayAllUsing strategy = Data.List.map theWinner $ Data.List.map (autoPlayFromUsing strategy) [1..9]

-- auto-play a single game, starting with supplied position, using default strategy
autoPlayFrom :: Position -> Board
autoPlayFrom start = autoPlay (makeThisMove start board)
  where board = newBoard

-- auto-play a single game, starting with supplied board (which may be partially played), using default strategy
autoPlay :: Board -> Board
autoPlay board = autoPlayUsing smarterMove board

-- auto-play a single game, starting with supplied position, using supplied strategy
autoPlayFromUsing :: Strategy -> Position -> Board
autoPlayFromUsing strategy start = autoPlayUsing strategy (makeMove start board)
  where board = newBoard
        player = whosMove board

-- auto-play a single game, starting with supplied board (which may be partially played), using supplied strategy
autoPlayUsing :: Strategy -> Board -> Board
autoPlayUsing strategy board
  | aWinner nextBoard = nextBoard
  | not $ hasUnplayed (squares nextBoard) = nextBoard
  | otherwise = autoPlayUsing strategy nextBoard
  where nextBoard = strategy board

-- auto-play a single game, starting with "head" of supplied boards, using default strategy
-- prepend board to list after each move
autoPlayTrack :: [Board] -> [Board]
autoPlayTrack boards = autoPlayUsingTrack smarterMove boards


-- auto-play a single game, starting with supplied position & strategy
-- prepend board to list after each move
autoPlayFromUsingTrack :: Strategy -> Position -> [Board]
autoPlayFromUsingTrack strategy start = autoPlayUsingTrack strategy ([makeMove start board])
  where board = newBoard
        player = whosMove board

-- auto-play a single game, starting with "head" of supplied boards, using supplied strategy
-- prepend board to list after each move
autoPlayUsingTrack :: Strategy -> [Board] -> [Board]
autoPlayUsingTrack strategy boards
  | aWinner nextBoard = nextBoard : boards
  | not $ hasUnplayed (squares nextBoard) = nextBoard : boards
  | otherwise = autoPlayUsingTrack strategy (nextBoard : boards)
  where nextBoard = strategy $ head boards

-- \ game play

-- / game strategies

-- given a board, try to make best next move
-- streamlined strategy ... this will autoplay every starting position to a draw
-- but can be defeated by a human with certain sequences
smarterMove :: Board -> Board
smarterMove board = makeMove loc board
    where  player = whosMove board
           loc = betterUnplayedSquare board player

-- from a list of squares, return the 1st unticked one of highest rank or 0
-- using more involved ranking
betterUnplayedSquare :: Board -> Player -> Int
betterUnplayedSquare b p
 | length possiblePositions == 0 = 0
 | otherwise = head possiblePositions
 where possiblePositions = rankUnplayedPositions b p

-- returns unplayed positions ranked by most tics for player in its intersections
rankUnplayedPositions :: Board -> Player -> [Position]
rankUnplayedPositions b p =
 Data.List.map nexus (sortBy (rankIntersectionFor p) (byIntersectionsUnplayed b))

-- if one intersection has a better score, it's better
-- if they're the same, rank by their position
-- ... all descending, which is why they look backwards
-- avoids (albeit minor in this case) reverse expense ... because I never want them ascending
rankIntersectionFor :: Player -> Intersection -> Intersection -> Ordering
rankIntersectionFor p i1 i2
  | i1Score > i2Score = LT
  | i1Score < i2Score = GT
  | i1Score == i2Score && i1Nexus > i2Nexus = LT
  | i1Score == i2Score && i1Nexus < i2Nexus = GT
  | otherwise = EQ
  where i1Score = scoreIntersectionFor p i1
        i2Score = scoreIntersectionFor p i2
        i1Nexus = nexus i1
        i2Nexus = nexus i2


-- this is the meat of the "smarter" strategy
scoreIntersectionFor :: Player -> Intersection -> Int
scoreIntersectionFor p i
 -- winner or loser, easy choice
 | elem Winner scoresMe || elem Loser scoresMe = 32
 -- magic square for me
 | (length $ Data.List.filter (== ForkableMe) scoresMe) > 1 = 30
 -- magic square for opponent
 | (length $ Data.List.filter (== ForkableOther) scoresMe) > 1 = 28
 -- it's open corner & opponent occupies opposite
 | unblocked && aCorner itsNexus && tic (squareAt opposite i) == op = 8
 -- it's an open centre
 | unblocked && theCentre itsNexus  = 10
 -- it's an open corner
 | unblocked && aCorner itsNexus  = 6
 -- it possess some other advantage ...
 | or $ Data.List.map (> Playable) scoresMe = 4
 -- well, it isn't blocked at least
 | unblocked = 2
 -- we're on our way to a draw
 | otherwise = 0
  where itsNexus = nexus i
        scoredMe = Data.List.map (\sql -> scoreSqListFor p sql) (rows i)
        scoresMe = Data.List.map fst scoredMe
        scoredOp = Data.List.map (\sql -> scoreSqListFor op sql) (rows i)
        scoresOp = Data.List.map fst scoredOp
        unblocked = or $ Data.List.map (> Blocked) scoresMe
        opposite = opposition itsNexus  -- "opposite position"
        op = otherPlayer p


occupiesAdjacentCorners :: Intersection -> Player -> Bool
occupiesAdjacentCorners i py = and (Data.List.map (\ac-> ((tic (squareAt ac i)))  == py) (adjacentCorners (nexus i)))

-- index a square out of an intersection's "rows"
-- caller knows it will be there, so doesn't protect against empty list
squareAt :: Position -> Intersection -> Square
squareAt  p i = head $ Data.List.filter (\i -> location i == p) sqs
  where sqs = concat (rows i)

adjacentCorners :: Position -> [Position]
adjacentCorners p = snd $ head $ Data.List.filter (\adj -> fst adj == p) adjs
  where adjs = [ (1,[3,7]), (2,[1,3]), (3,[1,9]), (4,[1,7]), (5, [1,3,7,9]), (6, [3,9]), (7, [1,9]), (8, [7,9]), (9, [3,7])]


--   for "rows" of squares ... logic makes no sense if this is a random collection of squares
scoreSqListFor :: Player -> [Square] -> (Score, [Square])
scoreSqListFor player sqs
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
        players = ticCount player sqs
        opponent = otherPlayer player
        opponents = ticCount opponent sqs


-- \ game strategies

-- / simple game strategy

smartMove :: Board -> Board
smartMove board = makeMove (pickUnplayedSquare $ head $ rankBoardRows board (whosMove board)) board

-- from a list of squares, return position of the 1st unticked one
-- using fairly simple ranking
pickUnplayedSquare :: [Square] -> Int
pickUnplayedSquare squares
  | length sqs == 0 = 0
  | otherwise = location $ head $ rankSquares sqs
  where sqs = Data.List.filter (\sq -> isUnplayed sq) squares


-- order "rows" by how 'good' they are for Player
rankBoardRows :: Board -> Player ->  [[Square]]
rankBoardRows board player = sortBy  (rankSqList player) (playableRows board)


-- by score ... descending (compare is backwards)
rankSqList :: Player -> [Square] -> [Square] -> Ordering
rankSqList player first second
  | score1st > score2nd = LT
  | score1st < score2nd = GT
  | otherwise = EQ
  where score1st = fst $ scoreSqListFor player first
        score2nd = fst $ scoreSqListFor player second

--- order squares by "rank" descending
rankSquares :: [Square] -> [Square]
rankSquares squares = sortBy rankSquare squares

rankSquare :: Square -> Square -> Ordering
rankSquare sq1 sq2 = rankPosition (location sq1) (location sq2)

-- \ simple game strategy



-- / square state functions

-- weights a collection of "rows", by summing player's tics for those rows not occuped by opponent
ticCountSumUseful :: Player -> [[Square]] -> Int
ticCountSumUseful player sqls = Data.List.foldr (+) 0 (Data.List.map (ticCount player) (Data.List.filter (isUnplayedFor (otherPlayer player)) sqls))

ticCount :: Player -> [Square] -> Int
ticCount player squares = length $ Data.List.filter (\a -> tic a == player) squares

-- squares with supplied positions
squaresFor :: Board -> [Position] -> [Square]
squaresFor b ps =  Data.List.map (squareFor b) ps

-- square with supplied position
-- this is meant to be the only place where board index == position - 1 matters
squareFor :: Board -> Position -> Square
squareFor b p =  (squares b) !! (p-1)

isUnplayedFor :: Player -> [Square] -> Bool
isUnplayedFor p squares = length (Data.List.filter (\sq -> tic sq == p) squares) == 0

hasUnplayed :: [Square] -> Bool
hasUnplayed squares = length (Data.List.filter (\sq -> tic sq == N) squares) > 0

isUnplayedPosition :: Board -> Position -> Bool
isUnplayedPosition b p = isUnplayed (squareFor b p)

isUnplayed :: Square -> Bool
isUnplayed square = tic square == N

-- \ square state functions


-- / board state functions

playableRows :: Board -> [[Square]]
playableRows board = Data.List.filter hasUnplayed (winningRows board)

-- given a board, return its winning combos
winningRows :: Board -> [[Square]]
winningRows board = Data.List.map (\w -> squaresFor board w) winners

-- given a position & board, return postion's winning combos from board
winnersFor :: Position -> Board -> [[Square]]
winnersFor thePosition board = (Data.List.filter (\w -> elem thePosition (Data.List.map location w))) (winningRows board)

byIntersectionsUnplayed :: Board -> [Intersection]
byIntersectionsUnplayed b =  Data.List.filter (\i -> isUnplayedPosition b (nexus i)) (byIntersections b)

-- represent a board as a list of intersections for each position
byIntersections :: Board -> [Intersection]
byIntersections  board = Data.List.map (\p -> Intersection p (winnersFor p board)) [1..9]

winner :: Board -> Player -> Bool
winner board player =  or $ Data.List.map (\w -> isInfixOf w ticked) winners
  where ticked = Data.List.map location (Data.List.filter (\sq -> (tic sq) == player) (squares board))


theWinner :: Board -> Player
theWinner board = fst $ whoWonMoves board

aWinner :: Board -> Bool
aWinner board = theWinner board /= N

-- who won & how many moves it took
--  ('/',9) == a draw
--  ('/', [0..8]) == an unfinished game
whoWonMoves :: Board -> (Player, Int)
whoWonMoves b
 | winner b X = (X, m)
 | winner b O = (O, m)
 | otherwise = (N, m)
 where m = movesMade b

movesMade :: Board -> Int
movesMade b = 9 - length (unplayedSquares (squares b))

-- give all moves made by winning player, not just winning sequence
howWon :: Board -> (Player, [Position])
howWon board = (winner, Data.List.map location sqForWinner)
  where winner = theWinner board
        sqForWinner = [sq | sq <- (squares board), tic sq == winner]

-- if board is empty, assumes 'x' plays first ...
whosMove :: Board -> Player
whosMove b
 | movesLeft == 0 = N
 | mod movesLeft 2 == 0 = O
 | otherwise = X
 where movesLeft = length (unplayedSquares (squares b))

whichMove :: Board -> Move
whichMove b = 10 - length (unplayedSquares (squares b))

boardMoves :: [Board] -> [Square]
boardMoves [] = []
boardMoves (bb:ba:bs) = (diffBoards ba bb) ++ boardMoves(ba:bs)
boardMoves (bb:bs)
 | length bs == 0 = []
 | otherwise = (diffBoards (head bs) bb)

diffBoards :: Board -> Board -> [Square]
diffBoards b1 b2 = diffSquares (squares b1) (squares b2)

diffSquares :: [Square] -> [Square] -> [Square]
diffSquares sqs1 sqs2 = toList (difference  (fromList sqs2) (fromList sqs1))

unplayedSquares :: [Square] -> [Square]
unplayedSquares b = Data.List.filter (\sq -> tic sq == N) b

playedSquares :: [Square] -> [Square]
playedSquares b = Data.List.filter (\sq -> tic sq /= N) b



otherPlayer :: Player -> Player
otherPlayer p
  | p == O = X
  | p == X = O
  | otherwise = N

-- \ board state functions

-- / mechanics

-- given a position, tic for next player
-- ignore if square is occupied
makeThisMove :: Position -> Board -> Board
makeThisMove p b
  | not $ (isUnplayed $ squareFor b p) = b
  | otherwise = makeMove p b


makeMove :: Position -> Board -> Board
makeMove loc board
  | loc == 0 = board
  | loc == 1 = Board (square : rightBoard)
  | loc == 9 = Board ((init (squares board)) ++ [square])
  | otherwise = Board (init leftBoard ++ [square] ++ rightBoard)
  where (leftBoard, rightBoard) = splitAt loc (squares board)
        square = Square loc (whosMove board) (whichMove board)

-- \ mechanics

-- / programmed play ... useful for testing

-- play a supplied sequence of moves, alternating players
playMoves :: [Position] -> Board
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
  | otherwise = moveThrough (tail unplayedSquares, (makeThisMove (head unplayedSquares) board))

-- \ programmed play

-- / util

removeDupes :: String -> String
removeDupes [] = []
removeDupes (x:y:xs)
  | x == y =  removeDupes (y:xs)
  | otherwise = x:(removeDupes (y:xs))
removeDupes s = s

fullRange :: (Bounded a, Enum a) => [a]
fullRange = [minBound..maxBound]

players :: [Player]
players = fullRange