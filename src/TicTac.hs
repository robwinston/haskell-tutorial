module TicTac where

import Data.List
import Data.Maybe
import Data.Set
import MyLists

data Player = N | X | O
  deriving (Eq, Show, Ord, Bounded, Enum)

data Row = T | M | B
  deriving (Eq, Show, Ord, Bounded, Enum)
data Column = L | C | R
  deriving (Eq, Show, Ord, Bounded, Enum)

type Location = (Row, Column)

usableLocations :: [Location]
usableLocations = [(r,c)| r <- fullRange, c <- fullRange]

rankLocation :: Location -> Location -> Ordering
rankLocation p1 p2 = compare (itsRank p1) (itsRank p2)

opposite :: Location -> Location
opposite l
  | l == (T, L) = (B, R)
  | l == (T, C) = (B, C)
  | l == (T, R) = (B, L)
  | l == (M, L) = (M, R)
  | l == (M, C) = (M, C)
  | l == (M, R) = (M, L)
  | l == (B, L) = (T, R)
  | l == (B, C) = (T, C)
  | l == (B, R) = (T, L)

corners :: [Location]
corners = [ (T, L), (T, R), (B, L), (B, R) ]
centre :: [Location]
centre = [ (M, C) ]
others :: [Location]
others = [ (T, C), (B, C), (M, L), (M, R) ]

theCentre l = isRank Nexus l
aCorner l = isRank Corner l

winners :: [[Location]]
winners = [[(T, L), (T, C), (T, R)], [(M, L), (M, C), (M, R)], [(B, L), (B, C), (B, R)], [(T, L), (M, L), (B, L)], [(T, C), (M, C), (B, C)], [(T, R), (M, R), (B, R)], [(T, L), (M, C), (B, R)], [(T, R), (M, C), (B, L)]]

isWinner :: [Location] -> Bool
isWinner ls = elem ls winners

data Rank = Edge | Corner | Nexus
  deriving (Eq, Ord, Show, Bounded, Enum)

isRank :: Rank -> Location -> Bool
isRank r l = itsRank l == r

itsRank :: Location -> Rank
itsRank l
  | elem l corners = Corner
  | elem l centre = Nexus
  | otherwise = Edge


data Score =  Unplayable | Blocked | Playable | MaybeOther | MaybeMe | ForkableOther | ForkableMe | Loser | Winner
  deriving (Eq, Ord, Show, Bounded, Enum)

type Move = Int

-- / Square
data Square = Square {
                 location :: Location,
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
newBoard = Board (Data.List.map (\i -> Square i N 0) usableLocations)

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
showGame (Game {boards = bds}) = (gameString "Game sequence: \n" bds) ++ "Moves: " ++ movesMade
  where movesMade
          | length bds < 2 = "none"
          | otherwise = show $ movesList (head bds) (last bds)


gameString :: String -> [Board] -> String
gameString s [] = s ++ "No boards!"
gameString s (b:bds)
  | length bds == 0 = s ++ show b ++ "\n" ++ show (movesCount b) ++ " moves made\n"
  | otherwise = gameString (s ++ show b ++ "\n") bds

-- \ Game

type Strategy = (Board -> Board)
type Scorer = (Player -> Intersection -> Int)

data Intersection = Intersection  {
                      nexus :: Location,
                      rows :: [[Square]] }
                    deriving (Eq)
instance Show Intersection
  where show (Intersection l r) = "(" ++ show l ++ "," ++ show r ++ ")\n"

-- / game play

play :: Board -> Maybe Location -> Board
play board location = playUsing smarterMove board location

playUsing :: Strategy -> Board -> Maybe Location -> Board
playUsing  strategy board location
  | aWinner board = board
  | location == Nothing = strategy board
  | otherwise = makeSuppliedMove (fromJust location) board

playARound :: Board -> Location -> Board
playARound board location = playARoundUsing smarterMove board location

playARoundUsing  :: Strategy -> Board -> Location -> Board
playARoundUsing strategy board location
  | aWinner board = board
  | aWinner nextBoard = nextBoard
  | otherwise = strategy nextBoard
  where nextBoard = firstMove board location
        firstMove b l = makeSuppliedMove l b


{-
ghci> autoPlayAllUsing smartMove
[N,N,O,N,X,N,N,N,N]
ghci> autoPlayAllUsing smarterMove
[N,N,N,N,N,N,N,N,N]
ghci>
-}

-- for all of the auto-play strategies accepting a starting board -
-- if the supplied board is in an invalid state, results are unpredictable

-- auto-play from all possible starting positions, return list of winner for each game
autoPlayAllUsing :: Strategy -> [Player]
autoPlayAllUsing strategy = Data.List.map theWinner $ Data.List.map (autoPlayFromUsing strategy) usableLocations

-- auto-play a single game, starting with supplied location, using default strategy
autoPlayFrom :: Location -> Board
autoPlayFrom start = autoPlay (makeSuppliedMove start board)
  where board = newBoard

-- auto-play a single game, starting with supplied board (which may be partially played), using default strategy
autoPlay :: Board -> Board
autoPlay board = autoPlayUsing smarterMove board

-- auto-play a single game, starting with supplied location, using supplied strategy
autoPlayFromUsing :: Strategy -> Location -> Board
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


-- auto-play a single game, starting with supplied location & strategy
-- prepend board to list after each move
autoPlayFromUsingTrack :: Strategy -> Location -> [Board]
autoPlayFromUsingTrack strategy start = autoPlayUsingTrack strategy ([makeMove start board])
  where board = newBoard
        player = whosMove board

-- auto-play a single game, starting with "head" of supplied boards, using supplied strategy
-- prepend board to list after each move
autoPlayUsingTrack :: Strategy -> [Board] -> [Board]
autoPlayUsingTrack strategy [] =  autoPlayUsingTrack strategy [newBoard]
autoPlayUsingTrack strategy boards
  | aWinner nextBoard = nextBoard : boards
  | not $ hasUnplayed (squares nextBoard) = nextBoard : boards
  | otherwise = autoPlayUsingTrack strategy (nextBoard : boards)
  where nextBoard = strategy $ head boards

-- \ game play

-- / game strategy

-- given a board, try to make best next move
-- streamlined strategy ... this will autoplay every starting location to a draw
-- but can be defeated by a human with certain sequences
smarterMove :: Board -> Board
smarterMove board
    | isJust loc = makeMove (fromJust loc) board
    | otherwise = board
    where  player = whosMove board
           loc = betterUnplayedSquare board player

-- from a list of squares, return the 1st unticked one of highest rank or 0
-- using more involved ranking
betterUnplayedSquare :: Board -> Player -> Maybe Location
betterUnplayedSquare b p
 | length possibleLocations == 0 = Nothing
 | otherwise = Just (head possibleLocations)
 where possibleLocations = rankUnplayedLocations b p

-- returns unplayed positions ranked by most tics for player in its intersections
rankUnplayedLocations :: Board -> Player -> [Location]
rankUnplayedLocations b p =
 Data.List.map nexus (sortBy (rankIntersectionFor p) (byIntersectionsUnplayed b))

-- if one intersection has a better score, it's better
-- if they're the same, rank by their location
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

scoreIntersectionFor :: Player -> Intersection -> Int
scoreIntersectionFor p i
 -- winner or loser, easy choice
 | elem Winner scoresMe || elem Loser scoresMe = 32
 -- magic square for me
 | (length $ Data.List.filter (== ForkableMe) scoresMe) > 1 = 30
 -- magic square for opponent
 | (length $ Data.List.filter (== ForkableOther) scoresMe) > 1 = 28
 -- it's open corner & opponent occupies opposite
 | unblocked && aCorner itsNexus && tic (squareAt itsOpposite i) == op = 8
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
        scoredMe = Data.List.map (scoreSqListFor p) (rows i)
        scoresMe = Data.List.map fst scoredMe
        scoredOp = Data.List.map (scoreSqListFor op) (rows i)
        scoresOp = Data.List.map fst scoredOp
        unblocked = or $ Data.List.map (> Blocked) scoresMe
        itsOpposite = opposite itsNexus  -- "opposite location"
        op = otherPlayer p

-- will force opponent to play a square of no real use to them
--isForceMove :: Square -> [Squares] -> Player -> Bool



occupiesAdjacentCorners :: Intersection -> Player -> Bool
occupiesAdjacentCorners i py = and (Data.List.map (\ac-> (tic (squareAt ac i))  == py) (adjacentCorners (nexus i)))

-- index a square out of an intersection's "rows"
-- caller knows it will be there, so doesn't protect against empty list
squareAt :: Location -> Intersection -> Square
squareAt  l i = head $ Data.List.filter (\sq -> location sq == l) sqs
  where sqs = concat (rows i)

-- "next to" == adjacent && (in a winning sequence) - e.g. diagonals are only "next to" corners or centre

allNextTos :: [Location] -> [Square] -> [(Location, [Square])]
allNextTos _ [] = []
allNextTos ls sqs =  [(l, (nextTos sqs l)) | l <- ls]

-- retireve a list of squares "next to" a location
nextTos :: [Square] -> Location -> [Square]
nextTos [] _ = []
nextTos sqs l = Data.List.filter (\sq -> nextTo l (location sq)) sqs

-- are squares "next to"  one another?
nextToSq :: Square -> Square -> Bool
nextToSq sq1 sq2 = nextTo (location sq1) (location sq2)

-- are locations "next to"  one another?
nextTo :: Location -> Location -> Bool
nextTo l1 l2 = elem l2 (adjacentLocations l1)

-- perhaps these nextTo / adjacentCorners could be computed, but pattern match is easy

-- for a given location, what are its relevant (i.e. part of a winning sequence) contiguous locations?
adjacentLocations :: Location -> [Location]
adjacentLocations l
  | l == (T,L) = [(T,C),(M,L),(M,C)]
  | l == (T,C) = [(T,L),(T,R),(M,C)]
  | l == (T,R) = [(T,C),(M,R),(M,C)]
  | l == (M,L) = [(T,L),(B,L),(M,C)]
  | l == (M,C) = [(T,L),(T,R),(M,L),(M,R),(B,L),(B,R)]
  | l == (M,R) = [(T,R),(B,R),(M,C)]
  | l == (B,L) = [(M,L),(B,C),(M,C)]
  | l == (B,C) = [(B,L),(B,R),(M,C)]
  | l == (B,R) = [(M,R),(B,C),(M,C)]

-- for a given location,what are its adjacent corners?
adjacentCorners :: Location -> [Location]
adjacentCorners l
  | l == (T,L) = [(T,R),(B,L)]
  | l == (T,C) = [(T,L),(T,R)]
  | l == (T,R) = [(T,L),(B,R)]
  | l == (M,L) = [(T,L),(B,L)]
  | l == (M,C) = [(T,L),(T,R),(B,L),(B,R)]
  | l == (M,R) = [(T,R),(B,R)]
  | l == (B,L) = [(T,L),(B,R)]
  | l == (B,C) = [(B,L),(B,R)]
  | l == (B,R) = [(T,R),(B,L)]

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

-- / game strategy

-- / simple game strategy

smartMove :: Board -> Board
smartMove board
  | isJust loc = makeMove (fromJust loc) board
  | otherwise = board
  where loc = pickUnplayedSquare $ head $ rankBoardRows board (whosMove board)

-- from a list of squares, return location of the 1st unticked one
-- using fairly simple ranking
pickUnplayedSquare :: [Square] -> Maybe Location
pickUnplayedSquare squares
  | length sqs == 0 = Nothing
  | otherwise = Just (location $ head $ rankSquares sqs)
  where sqs = Data.List.filter isUnplayed squares


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
rankSquare sq1 sq2 = rankLocation (location sq1) (location sq2)

-- \ simple game strategy



-- / square state functions

-- weights a collection of "rows", by summing player's tics for those rows not occuped by opponent
ticCountSumUseful :: Player -> [[Square]] -> Int
ticCountSumUseful player sqls = Data.List.foldr (+) 0 (Data.List.map (ticCount player) (Data.List.filter (isUnplayedFor (otherPlayer player)) sqls))

ticCount :: Player -> [Square] -> Int
ticCount player squares = length $ Data.List.filter (\a -> tic a == player) squares

-- squares with supplied positions
squaresFor :: Board -> [Location] -> [Square]
squaresFor b ps =  Data.List.map (squareFor b) ps

-- square with supplied location
-- this is meant to be the only place where board index == location - 1 matters
squareFor :: Board -> Location -> Square
squareFor b l =  head $ Data.List.filter (\sq -> (location sq) == l)(squares b)

isUnplayedFor :: Player -> [Square] -> Bool
isUnplayedFor p squares = length (Data.List.filter (\sq -> tic sq == p) squares) == 0

hasUnplayed :: [Square] -> Bool
hasUnplayed squares = length (Data.List.filter (\sq -> tic sq == N) squares) > 0

isUnplayedLocation :: Board -> Location -> Bool
isUnplayedLocation b p = isUnplayed (squareFor b p)

isUnplayed :: Square -> Bool
isUnplayed square = tic square == N

sortByMove :: [Square] -> [Square]
sortByMove squares = sortBy byMove squares

byMove :: Square -> Square -> Ordering
byMove firstSq secondSq = compare firstMove secondMove
  where firstMove = move firstSq
        secondMove = move secondSq

-- \ square state functions


-- / board state functions

playableRows :: Board -> [[Square]]
playableRows board = Data.List.filter hasUnplayed (winningRows board)

-- given a board, return its winning combos
winningRows :: Board -> [[Square]]
winningRows board = Data.List.map (squaresFor board) winners

-- given a location & board, return postion's winning combos from board
winnersFor :: Location -> Board -> [[Square]]
winnersFor theLocation board = (Data.List.filter (\w -> elem theLocation (Data.List.map location w))) (winningRows board)

byIntersectionsUnplayed :: Board -> [Intersection]
byIntersectionsUnplayed b =  Data.List.filter (\i -> isUnplayedLocation b (nexus i)) (byIntersections b)

-- represent a board as a list of intersections for each location
byIntersections :: Board -> [Intersection]
byIntersections  board = Data.List.map (\l -> Intersection l (winnersFor l board)) usableLocations

byNextTos :: Board -> [(Location, [Square])]
byNextTos board = allNextTos (Data.List.map location sqs) sqs
  where sqs = squares board


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
 where m = movesCount b

movesCount :: Board -> Int
movesCount b = 9 - length (unplayedSquares (squares b))

movesList :: Board -> Board -> [Square]
movesList start finish =  sortByMove (diffBoards start finish)

-- give all moves made by winning player, not just winning sequence
howWon :: Board -> (Player, [Location])
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

-- given a location, tic for next player
-- ignore if square is occupied
makeSuppliedMove :: Location -> Board -> Board
makeSuppliedMove p b
  | not $ (isUnplayed $ squareFor b p) = b
  | otherwise = makeMove p b


makeMove :: Location -> Board -> Board
makeMove loc board = Board  (sort (square:[sq | sq <- squares board, location sq /= loc]))
  where square = Square loc (whosMove board) (whichMove board)

-- \ mechanics

-- / programmed play ... useful for testing

-- play a supplied sequence of moves, alternating players
playMoves :: [Location] -> Board
playMoves ps =  snd (moveThrough (ps, newBoard))


-- play pre-defined game # n, return the board when game ends
-- 362,880 possible game sequences (although there's only 26,830 distinct games)
playGame :: Int -> Board
playGame n =  snd (moveThrough (game, newBoard))
  where game = permu usableLocations !! mod n 362880


-- given a sequence of moves & a board,
-- play until someone wins or no more moves
moveThrough :: ([Location], Board) -> ([Location], Board)
moveThrough (unplayedSquares, board)
  | length unplayedSquares == 0 = (unplayedSquares, board)
  | aWinner board = (unplayedSquares, board)
  | otherwise = moveThrough (tail unplayedSquares, (makeSuppliedMove (head unplayedSquares) board))

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