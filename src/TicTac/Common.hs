module TicTac.Common where

import Data.List
import Data.Maybe
import Data.Ord
import  qualified Data.Set as DS
import  qualified Data.Map as DM


-- / Data types

data Player = N | X | O
  deriving (Eq, Show, Ord, Bounded, Enum, Read)

data Role = Human | Computer
  deriving (Eq, Show, Ord, Bounded, Enum, Read)

data Row = T | M | B
  deriving (Eq, Show, Ord, Bounded, Enum, Read)

data Column = L | C | R
  deriving (Eq, Show, Ord, Bounded, Enum, Read)

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
  where show (Square loc ply m) = "|" ++ show loc ++ ":" ++ show ply ++ ":" ++ show m ++ "|"


data Intersection = Intersection  {
                      nexus :: Location,
                      rows :: [[Square]],
                      tallys :: [[(Player, Int)]]
                    }
                    deriving (Eq)
instance Show Intersection
  where show (Intersection l rws tys) = "(" ++ show l ++ "," ++ show rws ++  show tys ++ ")\n"

data Board = Board  {
               squares :: [Square] }
             deriving (Eq)
instance Ord Board
  where compare b1@(Board sqs1) b2@(Board sqs2)  = compare (movesCount b1) (movesCount b2)

instance Show Board
  where show brd@(Board sqrs) = "\n" ++ (fst (squaresToGrid ("", sqrs))) ++ (boardState brd)
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
                          justTic sqr  = "|" ++ show (tic sqr) ++ "|"

data GameInfo = GameInfo {
            game :: Game,
            playedBy :: [(Player, Role)]
        }

instance Show GameInfo
  where show (GameInfo gme plyBy) = showGameInfo (GameInfo gme plyBy)

data Game = Game {
         boards :: [Board]
       }
       deriving (Eq)

instance Show Game
  where show (Game brds) = showGame (Game brds)


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

-- / GameInfo functions

gamesSummary :: [GameInfo] -> [String]
gamesSummary [] = []
gamesSummary (gi:gis) =
  show gi : gamesSummary gis

showGameInfo :: GameInfo -> String
showGameInfo gi
  | player outc == N = "Draw in " ++ (show $ moves outc) ++ " moves"
  | otherwise = (show $ roleForPlayer (player outc) (playedBy gi)) ++ " (" ++  (show $ player outc) ++ ") in " ++ (show $ moves outc) ++ " moves"
  where outc = gameOutcome $ game gi


roleForPlayer :: Player -> [(Player, Role)] -> Role
roleForPlayer ply roles = head $ [r | (p,r) <- roles, p == ply]


-- \ GameInfo functions


-- / Game functions

boardsByMove :: Game -> [(Int, Board)]
boardsByMove g = zip [0..] (boards g)

boardForMove :: Game -> Int -> Maybe Board
boardForMove g m
 | null bfm = Nothing
 | otherwise = Just (head bfm)
  where bfm = [brd | (mv,brd) <- (boardsByMove g), mv == m]

gamesFor :: [Game] -> Player -> [Game]
gamesFor games py = [g | g@(Game brds) <- games, (player $ gameOutcome g) == py]

aGameFrom :: [Board] -> Game
aGameFrom brds = Game (sort brds)  -- Board's ORD compares how many moves have been made, so this puts boards in play sequence

aGameInfoFrom :: Game -> Player -> GameInfo
aGameInfoFrom game ply = GameInfo game [(ply, Human), (otherPlayer ply, Computer)]

asGame :: Board -> Game
asGame brd = Game [newBoard, brd]

gamePlay :: Game -> (Player, [Square])
gamePlay (Game brds) = (whoWon $ last brds, movesList (head brds) (last brds))

-- for a game - return winner (N == Draw) & # of moves
gameOutcome :: Game -> Outcome
gameOutcome (Game brds) = boardOutcome $ last brds

showGame :: Game -> String
showGame ((Game brds)) = (gameString "Game sequence: \n" brds) ++ "Moves: " ++ movesMade ++ "\n"
  where movesMade
          | length brds < 2 = "none"
          | otherwise = show $ movesList (head brds) (last brds)
        gameString :: String -> [Board] -> String
        gameString str [] = str ++ "No boards!"
        gameString str (brd:brds)
          | null brds = str ++ show brd ++ "\n" ++ show (movesCount brd) ++ " move" ++ plural ++ " made\n"
          | otherwise = gameString (str ++ show brd ++ "\n") brds
          where plural = if (movesCount brd) /= 1 then "s" else ""

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
makeMove brd loc = Board  (sort $ square:[sqr | sqr <- squares brd, location sqr /= loc])
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
byIntersections  brd = map (\loc -> buildIntersection brd loc) definedLocations

byTallys :: Board -> [(Location, [[Tally]])]
byTallys brd = map asTally (byIntersections brd)

allTallys brd = concat $ [concat $ tys | (l,tys) <- byTallys brd]
allWinningTallys brd = nub $ filter (\(p,t) -> t == 3) $ allTallys brd
allWinningTallysFor brd ply = filter (\(p,t) -> p == ply) $ allWinningTallys brd

byIntersectionsMap :: Board -> DM.Map Location ([[Square]], [[Tally]])
byIntersectionsMap  brd = DM.fromList $  map (\(Intersection loc sqrs tlys) -> (loc,(sqrs, tlys))) $ map (\loc -> buildIntersection brd loc) definedLocations

buildIntersection brd loc = Intersection loc rows tallys
  where rows = winningCombos loc brd
        tallys = map countPlayersInEachRow rows

lookupIntersection :: Location -> (DM.Map Location ([[Square]],[[Tally]])) -> Maybe Intersection
lookupIntersection loc itcMap = itcMaybe
  where entryMaybe = DM.lookup loc itcMap
        itcMaybe
          | isJust entryMaybe = Just $ Intersection loc (fst $ fromJust entryMaybe) (snd $ fromJust entryMaybe)
          | otherwise = Nothing

-- intersections with supplied positions
intersectionsFor :: Board -> [Location] -> [Intersection]
intersectionsFor brd locs =  map (intersectionFor brd) locs

-- intersection with supplied location
intersectionFor :: Board -> Location -> Intersection
intersectionFor brd loc = fromJust $ lookupIntersection loc imap
  where imap = byIntersectionsMap brd

intersectionsForCorners :: Board -> [Intersection]
intersectionsForCorners brd = intersectionsFor brd corners

unplayedSquares :: Board -> [Square]
unplayedSquares brd = filter (\sqr -> tic sqr == N) (squares brd)

playedSquares :: Board -> [Square]
playedSquares brd = sortByMove $ filter (\sqr -> tic sqr /= N) (squares brd)

playableRows :: Board -> [[Square]]
playableRows brd = filter hasUnplayed (winningRows brd)

winningRows :: Board -> [[Square]]
winningRows brd = map (squaresFor brd) winners

hasEmptyRow :: Board -> Location -> Bool
hasEmptyRow brd loc = hasUntouchedRow tToCheck
    where  iToCheck = fromJust $ (find (\(Intersection li _ _) -> li == loc) (byIntersections brd))
           tToCheck = asTally iToCheck

whoWon :: Board -> Player
whoWon brd
 | isJust $ find (\(p,t) -> p == O) awt = O
 | isJust $ find (\(p,t) -> p == X) awt = X
 | otherwise = N
 where awt = allWinningTallys brd


isTheWinner :: Board -> Player -> Bool
isTheWinner brd ply =  not $ null $ allWinningTallysFor brd ply

-- give all moves made by winning player, not just winning sequence
howWon :: Board -> (Player, [Location])
howWon brd = (winner, map location sqForWinner)
  where winner = whoWon brd
        sqForWinner = sortByMove $ [sqr | sqr <- (squares brd), tic sqr == winner]

-- if board is empty, assumes 'X' plays first ...
whosMove :: Board -> Player
whosMove brd
 | movesLeft == 0 = N
 | mod movesLeft 2 == 0 = O
 | otherwise = X
 where movesLeft = length $ unplayedSquares brd

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


-- opponent has a tic in all rows
isBlocked :: Board -> Bool
isBlocked brd = null $ filter (==0) $ map (countForPlayer (otherPlayer $ whosMove brd)) $ countPlayersInPlayableRows brd

-- squares with supplied positions
squaresFor :: Board -> [Location] -> [Square]
squaresFor brd locs =  map (squareFor brd) locs

-- square with supplied location
squareFor :: Board -> Location -> Square
squareFor brd loc =  head $ filter (\sqr -> (location sqr) == loc)(squares brd)

squaresForCorners :: Board -> [Square]
squaresForCorners brd = squaresFor brd corners

cornerSquaresWithAdjCorners :: Board -> [(Square, [Square])]
cornerSquaresWithAdjCorners brd =  [(sqr, squaresFor brd (adjacentCorners (location sqr))) | sqr <- squaresForCorners brd]

-- \ Board functions

-- \ Square functions
diffSquares :: [Square] -> [Square] -> [Square]
diffSquares sqs1 sqs2 = DS.toList $ DS.difference  (DS.fromList sqs2) (DS.fromList sqs1)

-- weights a collection of "rows", by summing player's tics for those rows not occuped by opponent
ticCountSumUseful :: Player -> [[Square]] -> Int
ticCountSumUseful ply sqls = foldr (+) 0 (map (ticCount ply) (filter (isUnplayedFor (otherPlayer ply)) sqls))

ticCount :: Player -> [Square] -> Int
ticCount ply sqrs = length $ filter (\a -> tic a == ply) sqrs

isUnplayedFor :: Player -> [Square] -> Bool
isUnplayedFor ply sqrs = null $ filter (\sqr -> tic sqr == ply) sqrs

hasUnplayed :: [Square] -> Bool
hasUnplayed sqrs = not $ null $ filter (\sqr -> tic sqr == N) sqrs

isUnplayed :: Square -> Bool
isUnplayed sqr = tic sqr == N

sortByMove :: [Square] -> [Square]
sortByMove sqrs = sortBy byMove sqrs

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
asTally (Intersection loc sqss tlys) = (loc, tlys)

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

actualPlayers :: [Player]
actualPlayers = [X,O]


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
removeDupes str = str

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
countPlayersInEachRow sqrs = [(ply, (countPlayerInEachRow sqrs ply)) | ply <- players ]

countPlayerInEachRow :: [Square] -> Player -> Int
countPlayerInEachRow sqrs ply = length $ filter (\sqr -> tic sqr == ply) sqrs


