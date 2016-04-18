module TicTac where

import Data.List
import Data.Maybe
import MyLists

data Player = X | O | N
  deriving (Eq, Show)

type Position = Int
opposition :: Position -> Position
opposition p = 10 - p

-- / Square
data Square = Square {
                 location :: Position,
                 tic :: Player }
              deriving (Eq)

instance Show Square
  where show (Square l p) = showSquare (Square l p)
showSquare :: Square -> String
showSquare (Square {location = l, tic = p}) = "|" ++ show l ++ ":" ++ show p ++ "|"

-- \ Square


-- / Board
oppositeSq :: Board -> Square -> Square
oppositeSq board (Square p _)  = squareFor board p


data Board = Board  { squares :: [Square] }


instance Show Board
  where show (Board [sq])  = showBoard (Board [sq])
showBoard :: Board -> String
showBoard b = ""

-- \ Board


data Intersection = Intersection  {
                      nexus :: Position,
                      triples :: [[Square]] }
                    deriving (Eq, Show)



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

playAllUsingWinners :: (Board -> Board) -> [Player]
playAllUsingWinners strategy = map theWinner $ map (autoPlayFromUsing strategy) [1..9]

autoPlayFrom :: Position -> Board
autoPlayFrom start = autoPlay (nextMove start board)
  where board = newBoard

autoPlay :: Board -> Board
autoPlay board = autoPlayUsing smarterMove board


-- autoPlay a game, but pick starting move
-- keep track of play as it proceeds
autoPlayFromUsingTrack :: (Board -> Board) -> Position -> [Board]
autoPlayFromUsingTrack strategy start = autoPlayUsingTrack strategy ([move (Square start player) board])
  where board = newBoard
        player = whosMove board

-- autoPlay a game, using first of supplied boards which may or may not have moves already made
-- keep adding boards to list as game play proceeds
autoPlayUsingTrack :: (Board -> Board) -> [Board] -> [Board]
autoPlayUsingTrack strategy boards
  | aWinner nextBoard = nextBoard : boards
  | not $ hasUnplayed (squares nextBoard) = nextBoard : boards
  | otherwise = autoPlayUsingTrack strategy (nextBoard : boards)
  where nextBoard = strategy $ head boards

-- autoPlay a game employing supplied strategy, but pick starting move
autoPlayFromUsing :: (Board -> Board) -> Position -> Board
autoPlayFromUsing strategy start = autoPlayUsing strategy (move (Square start player) board)
  where board = newBoard
        player = whosMove board

-- autoPlay a game employing supplied strategy, using supplied board which may or may not have moves already made
autoPlayUsing :: (Board -> Board) -> Board -> Board
autoPlayUsing strategy board
  | aWinner nextBoard = nextBoard
  | not $ hasUnplayed (squares nextBoard) = nextBoard
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
    unplayedSquare = pickUnplayedSquare (squares board)

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
 map nexus (sortBy (rankIntersectionFor p) (byIntersectionsUnplayed b))

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
 | length (filter (\r -> ticCount p r == 2) itsTriples) > 0 = 12
 -- has an about to lose
 | length (filter (\r -> ticCount o r == 2) itsTriples) > 0 = 11
 -- it's an open corner & opponent occupies opposite corner
 -- (if opponent allowed to tick, creates two magic corners, making a block impossible)
 | elem itsNexus [1,3,7,9] && tic (squareAt opposite itsTriples) == otherPlayer p = 10
 -- is a "magic corner", i.e. has two triples with tics
 | length (filter (\r -> ticCount p r == 1) itsTriples) > 1 = 9
 -- is an opponent's "magic corner"
 | length (filter (\r -> ticCount o r == 1) itsTriples) > 1 = 8
 -- is centre or corner with "connected" tics for opponent
 | elem itsNexus [1,3,5,7,9] && length (filter (\r -> ticCount o r == 1) itsTriples) > 0 = 7
 -- is centre or corner with "connected" tics for self
 | elem itsNexus [1,3,5,7,9] && length (filter (\r -> ticCount p r == 1) itsTriples) > 0 = 6
 -- is the centre
 | itsNexus == 5 = 5
 -- is a corner
 | elem itsNexus [1,3,7,9] = 4
 -- otherwise, sum unblocked ticks, at this point,
 -- intutively suspect this will always be 2 or less (if reached), but haven't proved it
 | otherwise = ticCountSumUseful p itsTriples
 where itsTriples = triples i
       itsNexus = nexus i
       opposite = opposition itsNexus  -- "opposite position"
       o = otherPlayer p
       -- index a square out of an intersection's "triples"
       -- caller knows it will be there, so doesn't protect against empty list
       squareAt :: Position -> [[Square]] -> Square
       squareAt  p lsqs = head $ filter (\i -> location i == p) sqs
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
ticCountSumUseful player sqls = foldr (+) 0 (map (ticCount player) (filter (isUnplayedFor (otherPlayer player)) sqls))

ticCount :: Player -> [Square] -> Int
ticCount player squares = length $ filter (\a -> tic a == player) squares

ticSquareMaybe :: Player -> Maybe Square -> Maybe Square
ticSquareMaybe p msq
  | isNothing msq = msq
  | otherwise = Just (ticSquare p square)
  where square = fromJust msq

ticSquare :: Player -> Square -> Square
ticSquare player oldSquare = Square (location oldSquare) player

-- squares with supplied positions
squaresFor :: Board -> [Position] -> [Square]
squaresFor b ps =  map (squareFor b) ps

-- square with supplied position
-- this is meant to be the only place where board index == position - 1 matters
squareFor :: Board -> Position -> Square
squareFor b p =  (squares b) !! (p-1)

isUnplayedFor :: Player -> [Square] -> Bool
isUnplayedFor p squares = length (filter (\sq -> tic sq == p) squares) == 0

hasUnplayed :: [Square] -> Bool
hasUnplayed squares = length (filter (\sq -> tic sq == N) squares) > 0

isUnplayedPosition :: Board -> Position -> Bool
isUnplayedPosition b p = isUnplayed (squareFor b p)

isUnplayed :: Square -> Bool
isUnplayed square = tic square == N

-- \ square state functions


-- / board state functions

newBoard :: Board
newBoard = Board (map (\i -> Square i N) [1..9])

playableTriples :: Board -> [[Square]]
playableTriples board = filter hasUnplayed (winningTriples board)

-- given a board, return its winning combos
winningTriples :: Board -> [[Square]]
winningTriples board = map (\w -> squaresFor board w) winners
  where  winners = [[1,2,3], [4,5,6], [7,8,9], [1,4,7], [2,5,8], [3,6,9], [1,5,9], [3,5,7]]

-- given a position & board, return postion's winning combos from board
winnersFor :: Position -> Board -> [[Square]]
winnersFor thePosition board = (filter (\w -> elem thePosition (map location w))) (winningTriples board)

byIntersectionsUnplayed :: Board -> [Intersection]
byIntersectionsUnplayed b =  filter (\i -> isUnplayedPosition b (nexus i)) (byIntersections b)

-- represent a board as a list of intersections for each position
byIntersections :: Board -> [Intersection]
byIntersections  board = map (\p -> Intersection p (winnersFor p board)) [1..9]

winner :: Board -> Player -> Bool
winner board player =  or $ map (\w -> isInfixOf w ticked) winners
  where ticked = map location (filter (\sq -> (tic sq) == player) (squares board))
        winners = [[1,2,3], [4,5,6], [7,8,9], [1,5,9], [3,5,7]]

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
 where m = 9 - length (unplayedSquares (squares b))

-- give all moves made by winning player, not just winning sequence
howWon :: Board -> (Player, [Position])
howWon board = (winner, map location sqForWinner)
  where winner = theWinner board
        sqForWinner = [sq | sq <- (squares board), tic sq == winner]

-- if board is empty, assumes 'x' plays first ...
whosMove :: Board -> Player
whosMove b
 | movesLeft == 0 = N
 | mod movesLeft 2 == 0 = O
 | otherwise = X
 where movesLeft = length (unplayedSquares (squares b))

unplayedSquares :: [Square] -> [Square]
unplayedSquares b = filter (\sq -> tic sq == N) b


otherPlayer :: Player -> Player
otherPlayer p
  | p == O = X
  | p == X = O
  | otherwise = N

-- \ board state functions

-- / mechanics

-- given a position, tic for next player
-- ignore if square is occupied
nextMove :: Position -> Board -> Board
nextMove p b
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
  | otherwise = moveThrough (tail unplayedSquares, (nextMove (head unplayedSquares) board))

-- \ programmed play

