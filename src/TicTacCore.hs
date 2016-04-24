module TicTacCore where

import Data.List
import Data.Maybe
import Data.Ord
import  qualified Data.Set as DS


-- / Data types

data Player = N | X | O
  deriving (Eq, Show, Ord, Bounded, Enum)
data Row = T | M | B
  deriving (Eq, Show, Ord, Bounded, Enum)

data Column = L | C | R
  deriving (Eq, Show, Ord, Bounded, Enum)

data Rank = Edge | Corner | Nexus
  deriving (Eq, Ord, Show, Bounded, Enum)


type Location = (Row, Column)
type Move = Int
type Strategy = (Board -> Board)

-- represents tics for a player
type Tally = (Player,Int)

data Square = Square {
                 location :: Location,
                 tic :: Player,
                 move :: Move }
              deriving (Eq, Ord)
instance Show Square
  where show square@(Square loc ply m) = "|" ++ show loc ++ ":" ++ show ply ++ ":" ++ show m ++ "|"


data Intersection = Intersection  {
                      nexus :: Location,
                      rows :: [[Square]]
                    }
                    deriving (Eq)
instance Show Intersection
  where show (Intersection n rs) = "(" ++ show n ++ "," ++ show rs ++  ")\n"

data Board = Board  {
               squares :: [Square] }
             deriving (Eq)
instance Ord Board
  where compare b1@(Board sqs1) b2@(Board sqs2)  = compare (movesCount b1) (movesCount b2)

instance Show Board
  where show brd@(Board sqs) = "\n" ++ (fst (squaresToGrid ("", sqs))) ++ (boardState brd)
            where boardState :: Board -> String
                  boardState brd
                    | aWinner brd = mvs ++ (show $ whoWon brd)  ++ " wins!\n"
                    | whosMove brd == N = mvs ++ "It's a draw\n"
                    | otherwise = mvs ++ (show $ whosMove brd) ++ " to move" ++ blocked ++ "\n"
                    where mvs = (show $ movesCount brd) ++ ": "
                          blocked = if isBlocked brd then " - blocked" else ""

                  squaresToGrid :: (String , [Square]) -> (String, [Square])
                  squaresToGrid (gridString, squares)
                    | null squares = (gridString, squares)
                    | otherwise =  squaresToGrid ((gridString ++ (removeDupes (concat (map justTic row))) ++ "\n"), whatsLeft)
                    where (row, whatsLeft) = splitAt 3 squares
                          justTic :: Square -> String
                          justTic square  = "|" ++ show (tic square) ++ "|"


data Game = Game {
         boards :: [Board]
       }
       deriving (Eq)

instance Show Game
  where show (Game bds) = showGame (Game bds)

showGame :: Game -> String
showGame ((Game bds)) = (gameString "Game sequence: \n" bds) ++ "Moves: " ++ movesMade ++ "\n"
  where movesMade
          | length bds < 2 = "none"
          | otherwise = show $ movesList (head bds) (last bds)
        gameString :: String -> [Board] -> String
        gameString s [] = s ++ "No boards!"
        gameString s (brd:bds)
          | null bds = s ++ show brd ++ "\n" ++ show (movesCount brd) ++ " moves made\n"
          | otherwise = gameString (s ++ show brd ++ "\n") bds
instance Ord Game
  where compare g1@(Game bds1) g2@(Game sqs2) = compare (gameOutcome g1) (gameOutcome g2)


data Outcome = Outcome {
                 player :: Player,
                 moves :: Int
               }
              deriving (Eq)
instance Show Outcome
  where show outc@(Outcome ply mvs) = "(" ++ show ply ++ " in " ++ show mvs ++ " moves)"
instance Ord Outcome
  where compare o1@(Outcome p1 m1) o2@(Outcome p2 m2)
           -- aWinner is greater than a draw
          | p1 /= N && p2 == N = GT
          | p1 == N && p2 /= N = LT
           -- if both winners, least moves is better
          | otherwise = compare (Down m1) (Down m2)


data Score =  Unplayable | Blocked | Playable | MaybeOther | MaybeMe | ForkableOther | ForkableMe | Loser | Winner
  deriving (Eq, Ord, Show, Bounded, Enum)

-- \ Data types

-- / Game functions

boardsByMove :: Game -> [(Int, Board)]
boardsByMove g = zip [0..] (boards g)

boardForMove :: Game -> Int -> Maybe Board
boardForMove g m
 | null bfm = Nothing
 | otherwise = Just (head bfm)
  where bfm = [brd | (mv,brd) <- (boardsByMove g), mv == m]

gamesFor :: [Game] -> Player -> [Game]
gamesFor games py = [g | g@(Game bds) <- games, (player $ gameOutcome g) == py]

aGameFrom :: [Board] -> Game
aGameFrom bds = Game (sort bds)  -- Board's ORD compares how many moves have been made

asGame :: Board -> Game
asGame brd = Game [newBoard, brd]

gamePlay :: Game -> (Player, [Square])
gamePlay (Game bds) = (whoWon $ last bds, movesList (head bds) (last bds))

-- for a game - return winner (N == Draw) & # of moves
gameOutcome :: Game -> Outcome
gameOutcome (Game bds) = boardOutcome $ last bds

-- \ Game functions

-- / Board functions

-- / mechanics
-- given a location, tic for next player
-- ignore if square is occupied
makeSuppliedMove :: Board -> Location -> Board
makeSuppliedMove brd ply
  | not $ (isUnplayed $ squareFor brd ply) = brd
  | otherwise = makeMove brd ply

-- replace square with new one & sort to preserve row-major order
makeMove :: Board -> Location -> Board
makeMove brd loc = Board  (sort $ square:[sq | sq <- squares brd, location sq /= loc])
  where square = Square loc (whosMove brd) (whichMove brd)
-- \ mechanics

movesCount :: Board -> Int
movesCount brd = 9 - length (unplayedSquares brd)

whichMove :: Board -> Move
whichMove brd = 10 - (length $ unplayedSquares brd)

movesList :: Board -> Board -> [Square]
movesList start finish =  sortByMove (diffBoards start finish)

allLocations :: Board -> [Location]
allLocations brd = map location $ squares brd

unplayedLocations :: Board -> [Location]
unplayedLocations brd = map location $ filter isUnplayed $ squares brd

-- represent a board as a list of lists of tallys for each unplayed location
unplayedTallys :: Board -> [(Location, [[Tally]])]
unplayedTallys  brd = map asTally (unplayedIntersections brd)

isUnplayedLocation :: Board -> Location -> Bool
isUnplayedLocation brd loc = elem loc $ unplayedLocations brd

unplayedIntersections :: Board -> [Intersection]
unplayedIntersections brd =  filter (\i -> isUnplayedLocation brd (nexus i)) (byIntersections brd)

byIntersections :: Board -> [Intersection]
byIntersections  brd = map (\loc -> Intersection loc (winningCombos loc brd)) definedLocations

unplayedSquares :: Board -> [Square]
unplayedSquares brd = filter (\sq -> tic sq == N) (squares brd)

playedSquares :: Board -> [Square]
playedSquares brd = sortByMove $ filter (\sq -> tic sq /= N) (squares brd)

playableRows :: Board -> [[Square]]
playableRows brd = filter hasUnplayed (winningRows brd)

winningRows :: Board -> [[Square]]
winningRows brd = map (squaresFor brd) winners

isUnoccupied :: [Location] -> Board -> [Location]
isUnoccupied locs brd  = [loc | (Intersection loc r) <- unplayedIntersections brd, elem loc locs]

hasEmptyRow :: Board -> Location -> Bool
hasEmptyRow brd loc = hasUntouchedRow tToCheck
    where  iToCheck = fromJust $ (find (\(Intersection li rsi) -> li == loc) (byIntersections brd))
           tToCheck = asTally iToCheck

whoWon :: Board -> Player
whoWon brd
 | winner brd X = X
 | winner brd O = O
 | otherwise = N

-- give all moves made by winning player, not just winning sequence
howWon :: Board -> (Player, [Location])
howWon brd = (winner, map location sqForWinner)
  where winner = whoWon brd
        sqForWinner = sortByMove $ [sq | sq <- (squares brd), tic sq == winner]

-- if board is empty, assumes 'X' plays first ...
whosMove :: Board -> Player
whosMove brd
 | movesLeft == 0 = N
 | mod movesLeft 2 == 0 = O
 | otherwise = X
 where movesLeft = length $ unplayedSquares brd

winner :: Board -> Player -> Bool
winner brd ply =  or $ map (\w -> isInfixOf w ticked) winners
  where ticked = map location (filter (\sq -> (tic sq) == ply) (squares brd))

-- given a location & board, return location's winning combos from board
winningCombos :: Location -> Board -> [[Square]]
winningCombos theLocation brd = (filter (\w -> elem theLocation (map location w))) (winningRows brd)

-- who won & how many moves it took
--  ('N',9) == a draw
--  ('N', [0..8]) == an unfinished game
boardOutcome :: Board -> Outcome
boardOutcome brd  = Outcome (whoWon brd) (movesCount brd)

aWinner :: Board -> Bool
aWinner brd = whoWon brd /= N

finished :: Board -> Bool
finished brd = (movesCount brd == 9) || (aWinner brd)

diffBoards :: Board -> Board -> [Square]
diffBoards b1 b2 = diffSquares (squares b1) (squares b2)

newBoard :: Board
newBoard = Board (map (\i -> Square i N 0) definedLocations)


--given a row major list of players, generate a "phony" (i.e. no move #'s) board for testing
--  1) cycle thru supplied player list to ensure there's enough to generate a board
--  2) invalid list will generate an invalid board
--  3) empty list employs the players function to generate one
boardFor :: [Player] -> Board
boardFor [] = Board [Square (snd pl) (fst pl)  0 | pl <- zip (players ++ (reverse players) ++ players) definedLocations ]
boardFor plys = Board [Square (snd pl) (fst pl)  0 | pl <- zip (cycle plys) definedLocations ]

-- opponent has a tic in all rows
isBlocked :: Board -> Bool
isBlocked brd = null $ filter (==0) $ map (countForPlayer (otherPlayer $ whosMove brd)) $ countPlayersInPlayableRows brd

-- squares with supplied positions
squaresFor :: Board -> [Location] -> [Square]
squaresFor brd ps =  map (squareFor brd) ps

-- square with supplied location
squareFor :: Board -> Location -> Square
squareFor brd loc =  head $ filter (\sq -> (location sq) == loc)(squares brd)

squaresForCorners :: Board -> [Square]
squaresForCorners brd = squaresFor brd corners

cornerSquaresWithAdjCorners :: Board -> [(Square, [Square])]
cornerSquaresWithAdjCorners brd =  [(sq, squaresFor brd (adjacentCorners (location sq))) | sq <- squaresForCorners brd]

-- \ Board functions

-- \ Square functions
diffSquares :: [Square] -> [Square] -> [Square]
diffSquares sqs1 sqs2 = DS.toList $ DS.difference  (DS.fromList sqs2) (DS.fromList sqs1)

-- weights a collection of "rows", by summing player's tics for those rows not occuped by opponent
ticCountSumUseful :: Player -> [[Square]] -> Int
ticCountSumUseful ply sqls = foldr (+) 0 (map (ticCount ply) (filter (isUnplayedFor (otherPlayer ply)) sqls))

ticCount :: Player -> [Square] -> Int
ticCount ply squares = length $ filter (\a -> tic a == ply) squares

isUnplayedFor :: Player -> [Square] -> Bool
isUnplayedFor ply squares = null $ filter (\sq -> tic sq == ply) squares

hasUnplayed :: [Square] -> Bool
hasUnplayed squares = not $ null $ filter (\sq -> tic sq == N) squares

isUnplayed :: Square -> Bool
isUnplayed square = tic square == N

sortByMove :: [Square] -> [Square]
sortByMove squares = sortBy byMove squares

byMove :: Square -> Square -> Ordering
byMove firstSq secondSq = compare firstMove secondMove
  where firstMove = move firstSq
        secondMove = move secondSq

-- / Square functions

-- / Location functions

definedLocations :: [Location]
definedLocations = [(r,c)| r <- fullRange, c <- fullRange]

maybeLocation :: Int -> Maybe Location
maybeLocation i
  | i < 1 || i > length ls = Nothing
  | otherwise = Just (ls !! (i - 1))
  where ls = definedLocations

rankLocation :: Location -> Location -> Ordering
rankLocation p1 p2 = compare (itsRank p1) (itsRank p2)

-- if it's not a corner, it doesn't have an opposite
oppositeCorner :: Location -> Maybe Location
oppositeCorner loc
  | loc == (T, L) = Just (B, R)
  | loc == (T, R) = Just (B, L)
  | loc == (B, L) = Just (T, R)
  | loc == (B, R) = Just (T, L)
  | otherwise = Nothing

opposite :: Location -> Location
opposite loc
  | loc == (T, L) = (B, R)
  | loc == (T, C) = (B, C)
  | loc == (T, R) = (B, L)
  | loc == (M, L) = (M, R)
  | loc == (M, C) = (M, C)
  | loc == (M, R) = (M, L)
  | loc == (B, L) = (T, R)
  | loc == (B, C) = (T, C)
  | loc == (B, R) = (T, L)

corners :: [Location]
corners = [ (T, L), (T, R), (B, L), (B, R) ]
centre :: [Location]
centre = [ (M, C) ]
middles :: [Location]
middles = [ (T, C), (B, C), (M, L), (M, R) ]
diagonals :: [[Location]]
diagonals = [[(T,L), (M,C), (B,R)], [(T,R), (M,C), (B,L)]]

theCentre :: Location -> Bool
theCentre loc = isRank Nexus loc

aCorner :: Location -> Bool
aCorner loc = isRank Corner loc

winners :: [[Location]]
winners = [[(T, L), (T, C), (T, R)], [(M, L), (M, C), (M, R)], [(B, L), (B, C), (B, R)], [(T, L), (M, L), (B, L)], [(T, C), (M, C), (B, C)], [(T, R), (M, R), (B, R)], [(T, L), (M, C), (B, R)], [(T, R), (M, C), (B, L)]]

-- for a given location, what are its relevant (i.e. part of a winning sequence) contiguous locations?
adjacentLocations :: Location -> [Location]
adjacentLocations loc
  | loc == (T,L) = [(T,C),(M,L),(M,C)]
  | loc == (T,C) = [(T,L),(T,R),(M,C)]
  | loc == (T,R) = [(T,C),(M,R),(M,C)]
  | loc == (M,L) = [(T,L),(B,L),(M,C)]
  | loc == (M,C) = [(T,L),(T,R),(M,L),(M,R),(B,L),(B,R)]
  | loc == (M,R) = [(T,R),(B,R),(M,C)]
  | loc == (B,L) = [(M,L),(B,C),(M,C)]
  | loc == (B,C) = [(B,L),(B,R),(M,C)]
  | loc == (B,R) = [(M,R),(B,C),(M,C)]

-- for a given location,what are its adjacent corners?
adjacentCorners :: Location -> [Location]
adjacentCorners loc
  | loc == (T,L) = [(T,R),(B,L)]
  | loc == (T,C) = [(T,L),(T,R)]
  | loc == (T,R) = [(T,L),(B,R)]
  | loc == (M,L) = [(T,L),(B,L)]
  | loc == (M,C) = [(T,L),(T,R),(B,L),(B,R)]
  | loc == (M,R) = [(T,R),(B,R)]
  | loc == (B,L) = [(T,L),(B,R)]
  | loc == (B,C) = [(B,L),(B,R)]
  | loc == (B,R) = [(T,R),(B,L)]

locationBetween :: Location -> Location -> Maybe Location
locationBetween loc1 loc2
  | not $ areInARow [loc1,loc2] = Nothing
  | areAdjacent loc1 loc2 = Nothing
  | length adjs == 1 = Just $ head adjs
  | length exceptCentre == 1 = Just $ head exceptCentre
  | otherwise = Nothing
  where aLoc1 = adjacentLocations loc1
        aLoc2 = adjacentLocations loc2
        adjs = intersect aLoc1 aLoc2
        exceptCentre = diffs adjs centre

areInARow :: [Location] -> Bool
areInARow locs =  shareRow || shareCol || inADiagonal
  where ll = length locs
        shareRow = (length $ nub [r | (r,c) <- locs]) == 1
        shareCol = (length $ nub [c | (r,c) <- locs]) == 1
        inADiagonal = elem ll (map (\diag -> length $ intersect locs diag) diagonals)

areAdjacent :: Location -> Location -> Bool
areAdjacent loc1 loc2 =  (elem loc1 (adjacentLocations loc2)) || (elem loc2 (adjacentLocations loc1))

shareRow :: Location -> [Location]
shareRow loc@(row,col) = diffs [l | l@(r,c) <- definedLocations, r == row] [loc]

shareColumn :: Location -> [Location]
shareColumn loc@(row,col) = diffs [l | l@(r,c) <- definedLocations, c == col] [loc]

shareDiag :: Location -> [Location]
shareDiag loc = []

isWinner :: [Location] -> Bool
isWinner ls = elem ls winners

isRank :: Rank -> Location -> Bool
isRank r loc = itsRank loc == r

itsRank :: Location -> Rank
itsRank loc
  | elem loc corners = Corner
  | elem loc centre = Nexus
  | otherwise = Edge

-- \ Location functions

-- / Intersection functions

-- Intersection: ((M,L),[[|(M,L):N:0|,|(M,C):N:0|,|(M,R):N:0|],[|(T,L):X:1|,|(M,L):N:0|,|(B,L):N:0|]])
-- Tally:        ((M,L),[[(N,3),(X,0),(O,0)],[(N,2),(X,1),(O,0)]])

asTally :: Intersection -> (Location, [[Tally]])
asTally (Intersection loc sqss) = (loc, map countPlayersInEachRow sqss)

hasUntouchedRow :: (Location, [[Tally]]) -> Bool
hasUntouchedRow (loc, tyss) = not $ null $ filter (\(py,ct) -> py == N && ct == 3) (concat tyss)

-- \ Intersection functions

-- / Player functions

otherPlayer :: Player -> Player
otherPlayer ply
  | ply == O = X
  | ply == X = O
  | otherwise = N

players :: [Player]
players = fullRange

-- \ Player functions

-- / Outcome functions

winnersFor :: [Outcome] -> Player -> Int
winnersFor outcomes ply = length $ [py | (Outcome py _) <- outcomes, py == ply]

-- \ Outcome functions

-- / util

diffs :: (Ord a ) => [a] -> [a] -> [a]
diffs l1 l2 = DS.toList $ DS.difference  (DS.fromList l1) (DS.fromList l2)

removeDupes :: String -> String
removeDupes [] = []
removeDupes (x:y:xs)
  | x == y =  removeDupes (y:xs)
  | otherwise = x:(removeDupes (y:xs))
removeDupes s = s

fullRange :: (Bounded a, Enum a) => [a]
fullRange = [minBound..maxBound]

permu :: [a] -> [[a]]
permu [] = [[]]
permu [x] = [[x]]
permu (x:xs) = concat (map (interl x) (permu xs))

interl :: a -> [a] -> [[a]]
interl a [] = [[a]]
interl a (x: xs) =
  (a:x:xs) : map (x:) (interl a xs)

-- \ util

-- [[(N,n),(X,n),(O,n)]]
countPlayersInPlayableRows :: Board -> [[(Player, Int)]]
countPlayersInPlayableRows brd = map countPlayersInEachRow (playableRows brd)

countForPlayer :: Player -> [(Player, Int)] -> Int
countForPlayer ply tallys = snd $ head $ filter (\t -> fst t == ply) tallys

countPlayersInEachRow :: [Square] -> [(Player, Int)]
countPlayersInEachRow sqs = [(ply, (countPlayerInEachRow sqs ply)) | ply <- players ]

countPlayerInEachRow :: [Square] -> Player -> Int
countPlayerInEachRow sqs ply = length $ filter (\sq -> tic sq == ply) sqs


