module TicTac where

import Data.List
import Data.Maybe
import Data.Set
import MyLists

data Player = N | X | O
  deriving (Eq, Ord, Show)

type Position = Int
opposition :: Position -> Position
opposition p = 10 - p

-- / Square
data Square = Square {
                 location :: Position,
                 tic :: Player }
              deriving (Eq, Ord)

instance Show Square
  where show (Square l p) = showSquare (Square l p)
showSquare :: Square -> String
showSquare (Square {location = l, tic = p}) = "|" ++ show l ++ ":" ++ show p ++ "|"

-- \ Square


-- / Board

data Board = Board  {
               squares :: [Square] }
             deriving (Eq)

newBoard :: Board
newBoard = Board (Data.List.map (\i -> Square i N) [1..9])

oppositeSq :: Board -> Square -> Square
oppositeSq board (Square p _)  = squareFor board p


instance Show Board
  where show (Board sqs)  = showBoard (Board sqs)
showBoard :: Board -> String
showBoard (Board {squares = sqs} ) =  (fst (squaresToGrid ("", sqs))) ++ (boardState b)
  where b = Board sqs
        boardState :: Board -> String
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

data Intersection = Intersection  {
                      nexus :: Position,
                      triples :: [[Square]] }
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
  | position < 1 || position > 9 = strategy $ strategy board
  | otherwise = strategy (makeThisMove position board)

{-
ghci> playAllUsingWinners smartMove
[N,N,N,N,N,N,N,X,X]  <- X wins when starting with position 8 or 9
ghci> playAllUsingWinners smarterMove
[N,N,N,N,N,N,N,N,N] <- plays to draw in all cases
ghci>
-}

playAllUsingWinners :: Strategy -> [Player]
playAllUsingWinners strategy = Data.List.map theWinner $ Data.List.map (autoPlayFromUsing strategy) [1..9]

autoPlayFrom :: Position -> Board
autoPlayFrom start = autoPlay (makeThisMove start board)
  where board = newBoard

autoPlay :: Board -> Board
autoPlay board = autoPlayUsing smarterMove board

-- autoPlay a game employing supplied strategy, but pick starting move
autoPlayFromUsing :: Strategy -> Position -> Board
autoPlayFromUsing strategy start = autoPlayUsing strategy (move (Square start player) board)
  where board = newBoard
        player = whosMove board

-- autoPlay a game employing supplied strategy, using supplied board which may or may not have moves already made
autoPlayUsing :: Strategy -> Board -> Board
autoPlayUsing strategy board
  | aWinner nextBoard = nextBoard
  | not $ hasUnplayed (squares nextBoard) = nextBoard
  | otherwise = autoPlayUsing strategy nextBoard
  where nextBoard = strategy board

autoPlayTrack :: [Board] -> [Board]
autoPlayTrack boards = autoPlayUsingTrack smarterMove boards


-- autoPlay a game, but pick starting move
-- keep track of play as it proceeds
autoPlayFromUsingTrack :: Strategy -> Position -> [Board]
autoPlayFromUsingTrack strategy start = autoPlayUsingTrack strategy ([move (Square start player) board])
  where board = newBoard
        player = whosMove board

-- autoPlay a game, using first of supplied boards which may or may not have moves already made
-- keep adding boards to list as game play proceeds
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
    unplayedSquare = pickUnplayedSquare (squares board)

-- from a list of squares, return the 1st unticked one of highest rank (or Nothing)
-- using fairly simple ranking
pickUnplayedSquare :: [Square] -> Maybe Square
pickUnplayedSquare squares
  | length sqs == 0 = Nothing
  | otherwise = Just (head $ rankSquares sqs)
  where sqs = Data.List.filter (\sq -> isUnplayed sq) squares

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
 Data.List.map nexus (sortBy (rankIntersectionFor p) (byIntersectionsUnplayed b))

-- if one intersection has a better score, it's better
-- if they're the same, rank by their position
-- ... all descending, which is why they look backwards
-- avoids (albeit minor in this case) reverse expense ... because I never want them ascending
rankIntersectionFor :: Player -> Intersection -> Intersection -> Ordering
rankIntersectionFor p i1 i2
  | i1Score > i2Score = LT
  | i1Score < i2Score = GT
  | i1Score == i2Score = rankPosition (nexus i1) (nexus i2)
  where i1Score = scoreIntersectionFor p i1
        i2Score = scoreIntersectionFor p i2


-- this is the meat of the "smarter" strategy
scoreIntersectionFor :: Player -> Intersection -> Int
scoreIntersectionFor p i
 -- has an about to win
 | length (Data.List.filter (\r -> ticCount p r == 2) itsTriples) > 0 = 12
 -- has an about to lose
 | length (Data.List.filter (\r -> ticCount o r == 2) itsTriples) > 0 = 11
 -- it's open corner & opponent occupies opposite
 | elem itsNexus [1,3,7,9] && tic (squareAt opposite i) == otherPlayer p = 10
 -- it's an open corner & opponent occupies adjacent corners
 -- (if opponent is allowed to tick, possibly creates two magic corners, making a block impossible)
 -- but, doesn't yet check if relevant squares are indeed open  <- this is why it can be tricked
 | elem itsNexus [1,3,7,9] && occupiesAdjacentCorners i (otherPlayer p) = 9
 -- is a "magic junction", i.e. has two triples with tics
 | length (Data.List.filter (\r -> ticCount p r == 1) itsTriples) > 1 = 8
 -- is an opponent's "magic junction"
 | length (Data.List.filter (\r -> ticCount o r == 1) itsTriples) > 1 = 7
 -- is the centre
 | itsNexus == 5 = 6
 -- is corner with "connected" tics for opponent
 | elem itsNexus [1,3,7,9] && length (Data.List.filter (\r -> ticCount o r == 1) itsTriples) > 0 = 5
 -- is corner with "connected" tics for self
 | elem itsNexus [1,3,7,9] && length (Data.List.filter (\r -> ticCount p r == 1) itsTriples) > 0 = 4
 -- is a corner
 | elem itsNexus [1,3,7,9] = 3
 -- otherwise, sum unblocked ticks, at this point,
 -- intutively suspect this will always be 2 or less (if reached), but haven't proved it
 | otherwise = ticCountSumUseful p itsTriples
 where itsTriples = triples i
       itsNexus = nexus i
       opposite = opposition itsNexus  -- "opposite position"
       o = otherPlayer p

occupiesAdjacentCorners :: Intersection -> Player -> Bool
occupiesAdjacentCorners i py = and (Data.List.map (\ac-> ((tic (squareAt ac i)))  == py) (adjacentCorners (nexus i)))
-- index a square out of an intersection's "triples"
-- caller knows it will be there, so doesn't protect against empty list
squareAt :: Position -> Intersection -> Square
squareAt  p i = head $ Data.List.filter (\i -> location i == p) sqs
  where sqs = concat (triples i)
adjacentCorners :: Position -> [Position]
adjacentCorners p = snd $ head $ Data.List.filter (\adj -> fst adj == p) adjs
  where adjs = [ (1,[3,7]), (2,[1,3]), (3,[1,9]), (4,[1,7]), (5, [1,3,7,9]), (6, [3,9]), (7, [1,9]), (8, [7,9]), (9, [3,7])]



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
rankSquare sq1 sq2 = rankPosition (location sq1) (location sq2)

-- by "value" of a position ... descending (compare is backwards)
-- centre -> a corner -> something else
-- Rank => Centre > Corner > Anything Else (which are themselves of equal rank)
rankPosition :: Position -> Position -> Ordering
rankPosition p1 p2
  | p1 == 5 = LT
  | p2 == 5 = GT
  | elem p1 [1,3,7,9] = LT
  | elem p2 [1,3,7,9] = GT
  | otherwise = EQ


-- \ game strategies


-- / square state functions

-- weights a collection of "triples", by summing player's tics for those triples not occuped by opponent
ticCountSumUseful :: Player -> [[Square]] -> Int
ticCountSumUseful player sqls = Data.List.foldr (+) 0 (Data.List.map (ticCount player) (Data.List.filter (isUnplayedFor (otherPlayer player)) sqls))

ticCount :: Player -> [Square] -> Int
ticCount player squares = length $ Data.List.filter (\a -> tic a == player) squares

ticSquareMaybe :: Player -> Maybe Square -> Maybe Square
ticSquareMaybe p msq
  | isNothing msq = msq
  | otherwise = Just (ticSquare p square)
  where square = fromJust msq

ticSquare :: Player -> Square -> Square
ticSquare player oldSquare = Square (location oldSquare) player

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

playableTriples :: Board -> [[Square]]
playableTriples board = Data.List.filter hasUnplayed (winningTriples board)

-- given a board, return its winning combos
winningTriples :: Board -> [[Square]]
winningTriples board = Data.List.map (\w -> squaresFor board w) winners

-- given a position & board, return postion's winning combos from board
winnersFor :: Position -> Board -> [[Square]]
winnersFor thePosition board = (Data.List.filter (\w -> elem thePosition (Data.List.map location w))) (winningTriples board)

byIntersectionsUnplayed :: Board -> [Intersection]
byIntersectionsUnplayed b =  Data.List.filter (\i -> isUnplayedPosition b (nexus i)) (byIntersections b)

-- represent a board as a list of intersections for each position
byIntersections :: Board -> [Intersection]
byIntersections  board = Data.List.map (\p -> Intersection p (winnersFor p board)) [1..9]

winner :: Board -> Player -> Bool
winner board player =  or $ Data.List.map (\w -> isInfixOf w ticked) winners
  where ticked = Data.List.map location (Data.List.filter (\sq -> (tic sq) == player) (squares board))

winners :: [[Position]]
winners = [[1,2,3], [4,5,6], [7,8,9], [1,4,7], [2,5,8], [3,6,9], [1,5,9], [3,5,7]]


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

showMoves :: [[Board]] -> [[Square]]
showMoves boards = []

diffBoards :: Board -> Board -> [Square]
diffBoards b1 b2 = diffSquares (squares b1) (squares b2)

diffSquares :: [Square] -> [Square] -> [Square]
diffSquares sqs1 sqs2 = toList (difference (fromList sqs1) (fromList sqs2))

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
  | otherwise = move (Square p player) b
  where player = whosMove b


moveMaybe :: Maybe Square -> Board -> Board
moveMaybe msq board
    | isNothing msq = board
    | otherwise = move (fromJust msq) board

move :: Square -> Board -> Board
move square board
  | itsLocation == 1 = Board (square : rightBoard)
  | itsLocation == 9 = Board ((init (squares board)) ++ [square])
  | otherwise = Board (init leftBoard ++ [square] ++ rightBoard)
  where itsLocation = location square
        sections = splitAt itsLocation (squares board)
        leftBoard = fst sections
        rightBoard = snd sections

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
