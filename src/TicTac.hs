module TicTac where

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
                    | length squares == 0 = (gridString, squares)
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
showGame (Game {boards = bds}) = (gameString "Game sequence: \n" bds) ++ "Moves: " ++ movesMade ++ "\n"
  where movesMade
          | length bds < 2 = "none"
          | otherwise = show $ movesList (head bds) (last bds)
        gameString :: String -> [Board] -> String
        gameString s [] = s ++ "No boards!"
        gameString s (brd:bds)
          | length bds == 0 = s ++ show brd ++ "\n" ++ show (movesCount brd) ++ " moves made\n"
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

-- / Player functions
otherPlayer :: Player -> Player
otherPlayer ply
  | ply == O = X
  | ply == X = O
  | otherwise = N

players :: [Player]
players = fullRange

-- \ Player functions

-- / Location functions
usableLocations :: [Location]
usableLocations = [(r,c)| r <- fullRange, c <- fullRange]

maybeLocation :: Int -> Maybe Location
maybeLocation i
  | i < 1 || i > length ls = Nothing
  | otherwise = Just (ls !! (i - 1))
  where ls = usableLocations

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
asTally :: Intersection -> (Location, [[Tally]])
asTally (Intersection loc sqss) = (loc, map countPlayersInEachRow sqss)

-- \ Intersection functions

-- / Board functions
newBoard :: Board
newBoard = Board (map (\i -> Square i N 0) usableLocations)


boardsByMove :: Game -> [(Int, Board)]
boardsByMove g = zip [0..] (boards g)

boardForMove :: Game -> Int -> Maybe Board
boardForMove g m
 | length bfm == 0 = Nothing
 | otherwise = Just (head bfm)
  where bfm = [brd | (mv,brd) <- (boardsByMove g), mv == m]


--given a row major list of players, generate a "phony" (i.e. no move #'s) board for testing
--  1) cycle thru supplied player list to ensure there's enough to generate a board
--  2) invalid list will generate an invalid board
--  3) empty list employs the players function to generate one
boardFor :: [Player] -> Board
boardFor [] = Board [Square (snd pl) (fst pl)  0 | pl <- zip (players ++ (reverse players) ++ players) usableLocations ]
boardFor plys = Board [Square (snd pl) (fst pl)  0 | pl <- zip (cycle plys) usableLocations ]

-- opponent has a tic in all rows
isBlocked :: Board -> Bool
isBlocked brd = (length $ filter (==0) $ map (countForPlayer (otherPlayer $ whosMove brd)) $ countPlayersInPlayableRows brd) == 0

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

-- / Game functions

aGameFrom :: [Board] -> Game
aGameFrom bds = Game (sort bds)  -- Board's ORD compares how many moves have been made

asGame :: Board -> Game
asGame brd = Game [newBoard, brd]

gamePlay :: Game -> (Player, [Square])
gamePlay Game{boards=bds} = (whoWon $ last bds, movesList (head bds) (last bds))

-- for a game - return winner (N == Draw) & # of moves
gameOutcome :: Game -> Outcome
gameOutcome Game{boards=bds} = boardOutcome $ last bds

-- \ Game functions

-- / Outcome functions
winnersFor :: [Outcome] -> Player -> Int
winnersFor outcomes ply = length $ [py | (Outcome py _) <- outcomes, py == ply]

-- \ Outcome functions

-- / Strategy functions

{-
ghci> strategyChecker smarterMove X
[(X,11),(O,282),(N,332)]
ghci> strategyChecker smarterMove O
[(X,114),(O,11),(N,51)]

ghci> strategyChecker smartMove X
[(X,19),(O,255),(N,407)]
ghci> strategyChecker smartMove O
[(X,54),(O,3),(N,35)]
ghci>
-}

strategyChecker :: Strategy -> Player -> [(Player, Int)]
strategyChecker s ply =  map (\py -> (py,(winnersFor outcomes py))) [X,O,N]
  where allGames = allPossibleGames s $ boardToUse ply
        outcomes = map gameOutcome allGames
        -- if X -> "human" plays first
        boardToUse X = newBoard
        -- if O -> computer plays first
        boardToUse O = s newBoard
        -- if N -> "human" plays first
        boardToUse N = newBoard

-- Given a strategy and a board, return all possible outcomes human v computer using supplied strategy
allPossibleGames :: Strategy -> Board -> [Game]
allPossibleGames s brd = map (aGameFrom) (playAllPossibleRounds s [[brd]])


playAllPossibleRounds :: Strategy -> [[Board]] -> [[Board]]
playAllPossibleRounds s  [] = playAllPossibleRounds s [[newBoard]]
playAllPossibleRounds s bdss
  | (length $ filter (\bds -> not $ finished $ head bds) bdss) == 0 = bdss
  | otherwise = playAllPossibleRounds s (concat $ map (\bds -> playPossibleRounds s bds) bdss)


-- for the head of a given board sequence, prepend all of the next possible rounds
-- where a round is
--  1) a specified move - representing a "human"
--  2) the computer's response (using specified strategy)
-- a given play is short-cicuited when a winner/draw is reached
playPossibleRounds :: Strategy -> [Board] -> [[Board]]
playPossibleRounds s bseq = (map (autoNextMove s) $ filter (\x -> not $ finished $ head x) bseqn)  ++ filter (\x -> finished $ head x)  bseqn
  where bseqn = playPossibles bseq

-- for the head of a given board sequence, prepend all of the n possible moves, yielding n board sequences
playPossibles :: [Board] -> [[Board]]
playPossibles (bb:bs)
  | finished bb = [bb:bs]
  | otherwise = map (\ba -> ba:(bb:bs)) nextMoves
  where upl = map location $ filter isUnplayed $ squares bb
        nextMoves = map (makeSuppliedMove bb) upl

-- for a list of boards, prepend next move using strategy
autoNextMove :: Strategy -> [Board] -> [Board]
autoNextMove  _ [] = []
autoNextMove s (brd:bs) = s brd : (brd:bs)

-- / game play

play :: Board -> Int -> Board
play brd i
 | aWinner brd = brd
 | isNothing loc  = smarterMove brd
 | otherwise = playl brd (fromJust loc)
 where loc = maybeLocation i


playl :: Board -> Location -> Board
playl brd loc
  | aWinner brd = brd
  | otherwise = makeSuppliedMove brd loc

playUsing :: Strategy -> Board -> Board
playUsing  s brd
  | aWinner brd = brd
  | otherwise = s brd


playARound :: Board -> Int -> Board
playARound brd i = playARoundUsing smarterMove brd i

playARoundUsing :: Strategy -> Board -> Int -> Board
playARoundUsing s brd i
 | aWinner brd = brd
 | aWinner nextBoard = nextBoard
 | otherwise = s nextBoard
 where ml = maybeLocation i
       nextBoard = firstMove brd ml
       firstMove brd ml
         | isNothing ml = s brd
         | otherwise = makeSuppliedMove  brd (fromJust ml)

{-
ghci> autoPlayAllUsing smartMove
[O,O,N,N,N,N,N,N,O]
ghci> autoPlayAllUsing smarterMove
[X,N,X,N,X,N,N,N,O]
-}
-- for all of the auto-play strategies accepting a starting board -
-- if the supplied board is in an invalid state, results are unpredictable

-- auto-play from all possible starting positions, return list of winner for each game
autoPlayAllUsing :: Strategy -> [Player]
autoPlayAllUsing strategy = map whoWon $ map (autoPlayFromUsing strategy) usableLocations

-- auto-play a single game, starting with supplied location, using default strategy
autoPlayFrom :: Location -> Board
autoPlayFrom start = autoPlay (makeSuppliedMove brd start)
  where brd = newBoard

-- auto-play a single game, starting with supplied board (which may be partially played), using default strategy
autoPlay :: Board -> Board
autoPlay brd = autoPlayUsing smarterMove brd

-- auto-play a single game, starting with supplied location, using supplied strategy
autoPlayFromUsing :: Strategy -> Location -> Board
autoPlayFromUsing strategy start = autoPlayUsing strategy (makeMove brd start)
  where brd = newBoard
        ply = whosMove brd

-- auto-play a single game, starting with supplied board (which may be partially played), using supplied strategy
autoPlayUsing :: Strategy -> Board -> Board
autoPlayUsing strategy brd
  | aWinner nextBoard = nextBoard
  | not $ hasUnplayed (squares nextBoard) = nextBoard
  | otherwise = autoPlayUsing strategy nextBoard
  where nextBoard = strategy brd

-- auto-play a single game, starting with "head" of supplied boards, using default strategy
-- prepend board to list after each move
autoPlayTrack :: [Board] -> [Board]
autoPlayTrack boards = autoPlayUsingTrack smarterMove boards


-- auto-play a single game, starting with supplied location & strategy
-- prepend board to list after each move
autoPlayFromUsingTrack :: Strategy -> Location -> [Board]
autoPlayFromUsingTrack strategy start = autoPlayUsingTrack strategy ([makeMove brd start])
  where brd = newBoard
        ply = whosMove brd

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

-- / refactored game strategy

-- for a list of Locations & its Tallys, a Player, and a "count" predicate
--  > return Locations where there's at least one Tally meeting the criterion
locationsForWhere :: [(Location, [[Tally]] )] -> Player -> (Int -> Bool) -> [Location]
locationsForWhere tys ply prd =
    map fst  (filter (\(loc,t) -> t > 0) (map (\(loc,ts) -> (loc, length $ filter (\t -> prd $ countForPlayer ply t) ts)) tys))

-- [[(N,n),(X,n),(O,n)]]
countPlayersInPlayableRows :: Board -> [[(Player, Int)]]
countPlayersInPlayableRows brd = map countPlayersInEachRow (playableRows brd)


countForPlayer :: Player -> [(Player, Int)] -> Int
countForPlayer ply tallys = snd $ head $ filter (\t -> fst t == ply) tallys

countPlayersInEachRow :: [Square] -> [(Player, Int)]
countPlayersInEachRow sqs = [(ply, (countPlayerInEachRow sqs ply)) | ply <- players ]

countPlayerInEachRow :: [Square] -> Player -> Int
countPlayerInEachRow sqs ply = length $ filter (\sq -> tic sq == ply) sqs

-- TODO collapse this once function signatures have been normalised
cleverMove :: Board -> Board
cleverMove  brd
  -- If opening move, play a corner
  | length openingMoves > 0 = makeMove brd (head openingMoves)
  -- Win: If the player has two in a row, the player plays the third to win
  | length winables > 0 = makeMove brd (head winables)
  -- Block: If the opponent has two in a row, the player plays the third to block
  | length losables > 0 = makeMove brd (head losables)
  -- Fork: Create an opportunity where the player has two threats to win (two non-blocked lines of 2).
  | length forkables > 0 = makeMove brd (head forkables)
  -- Block: force player or block their forkable
  | length blockables > 0 = makeMove brd (head blockables)
  -- Center: play centre
  | length ctrs > 0 = makeMove brd (head ctrs)
  -- Opposite: if oppenent occupies opposite corner, play it
  | length opcs > 0 = makeMove brd (head opcs)
  -- Open corner: if there's an open corner, play it
  | length ocs > 0 = makeMove brd (head ocs)
  -- Open middle: if there's an open middle, play it
  | length oms > 0 = makeMove brd (head oms)
  | length lup > 0 = makeMove brd (head lup)
  | otherwise = brd
  where openingMoves = openingMove brd
        winables = canWin (whosMove brd) brd
        losables = canWin (otherPlayer $ whosMove brd) brd
        forkables = canFork (whosMove brd) brd
        blockables = blocking brd
        ctrs = isUnoccupied  centre brd
        opcs = oppositeOccupied brd
        ocs = isUnoccupied  corners brd
        oms = isUnoccupied  middles brd
        lup = unplayedLocations brd


-- unplayed corners occupied by opponent
oppositeOccupied :: Board -> [Location]
oppositeOccupied brd = [loc | loc <- unplayedLocations brd, (elem loc corners)  && ((tic $ squareFor brd $ opposite loc) == oply)]
  where oply = otherPlayer $ whosMove brd

openingMove :: Board -> [Location]
openingMove brd
  | (length $ allLocations brd) - (length $ unplayedLocations brd)  == 0 = corners
  | otherwise = []


canWin :: Player -> Board -> [Location]
canWin ply brd  =
  map fst  (filter (\(loc,t) -> t > 0) (map (\(loc,ts) -> (loc, length $ filter (\t -> countForPlayer ply t == 2) ts)) (unplayedTallys brd)))

canFork :: Player -> Board -> [Location]
canFork ply brd  =
  map fst  (filter (\(loc,t) -> t > 1) (map (\(loc,ts) -> (loc, length $ filter (\t -> countForPlayer ply t == 1 && countForPlayer opy t == 0) ts)) (unplayedTallys brd)))
  where opy = otherPlayer ply

isUnoccupied :: [Location] -> Board -> [Location]
isUnoccupied locs brd  = [loc | Intersection{nexus=loc, rows=r} <- unplayedIntersections brd, elem loc locs]


{-
Fromal Snapshot 4:

If computer goes first, loses once ...
ghci> map (strategyChecker cleverMove) [X,O]
[[(X,0),(O,316),(N,141)],[(X,54),(O,1),(N,37)]]
ghci>

-}
blocking :: Board -> [Location]
blocking brd
  | length cornerBlock > 0 = cornerBlock
  | length forceToMiddle > 0 = forceToMiddle
  | length forkableByOpponent > 0 = forkableByOpponent
  | length forceableOnly > 0 = forceableOnly
  | otherwise = []
  where forceable = canForce ply brd
        forkableByOpponent = canFork opy brd
        inboth = intersect forceable forkableByOpponent
        forceableOnly = diffs forceable forkableByOpponent
        ply = whosMove brd
        opy = otherPlayer ply
        -- its a corner & it owns the other corner, so playing here will force oppoent to defend in middle
        --  (thereby keeping opponent from exploiting an opportunity)
        cornerBlock = [l | l <- inboth, (elem l corners) && (length (filter (\sq ->  (tic sq) == ply) (squaresFor brd (adjacentCorners l))) > 0)]
        forceToMiddle = [l | l <- forceable, (not $ elem l corners)]


canForce :: Player -> Board -> [Location]
canForce ply brd =
  map fst  (filter (\(loc,t) -> t > 0) (map (\(loc,ts) -> (loc, length $ filter (\t -> countForPlayer ply t == 1 && countForPlayer opy t == 0) ts)) (unplayedTallys brd)))
  where opy = otherPlayer ply

diffs :: (Ord a ) => [a] -> [a] -> [a]
diffs l1 l2 = DS.toList $ DS.difference  (DS.fromList l1) (DS.fromList l2)

-- \ refactored game strategy

-- / game strategy

-- given a board, try to make best next move
-- streamlined strategy ... this will autoplay every starting location to a draw
-- but can be defeated by a human with certain sequences
smarterMove :: Board -> Board
smarterMove brd
    | isJust loc = makeMove brd (fromJust loc)
    | otherwise = brd
    where  ply = whosMove brd
           loc = betterUnplayedSquare brd ply

-- from a list of squares, return the 1st unticked one of highest rank or 0
-- using more involved ranking
betterUnplayedSquare :: Board -> Player -> Maybe Location
betterUnplayedSquare brd ply
 | length possibleLocations == 0 = Nothing
 | otherwise = Just (head possibleLocations)
 where possibleLocations = rankUnplayedLocations brd ply

-- returns unplayed positions ranked by most tics for player in its intersections
rankUnplayedLocations :: Board -> Player -> [Location]
rankUnplayedLocations brd ply =
 map nexus (sortBy (rankIntersectionFor ply) (unplayedIntersections brd))

-- if one intersection has a better score, it's better
-- if they're the same, rank by their location
-- ... all descending, which is why they look backwards
-- avoids (albeit minor in this case) reverse expense ... because I never want them ascending
rankIntersectionFor :: Player -> Intersection -> Intersection -> Ordering
rankIntersectionFor ply i1 i2
  | i1Score > i2Score = LT
  | i1Score < i2Score = GT
  | i1Score == i2Score && i1Nexus > i2Nexus = LT
  | i1Score == i2Score && i1Nexus < i2Nexus = GT
  | otherwise = EQ
  where i1Score = scoreIntersectionFor ply i1
        i2Score = scoreIntersectionFor ply i2
        i1Nexus = nexus i1
        i2Nexus = nexus i2

scoreIntersectionFor :: Player -> Intersection -> Int
scoreIntersectionFor ply i
 -- winner or loser, easy choice
 | elem Winner scoresMe = 34
 | elem Loser scoresMe = 32
 -- force for me
 | length myNextTos > 1 = 30
 -- magic square for me
 | (length $ filter (== ForkableMe) scoresMe) > 1 = 28
 -- force for opponent
 | length opNextTos > 1 = 26
 -- magic square for opponent
 | (length $ filter (== ForkableOther) scoresMe) > 1 = 24
 -- it's an open centre
 | unblocked && theCentre itsNexus  = 10
 -- it's open corner & opponent occupies opposite
 | unblocked && aCorner itsNexus && tic (squareAt itsOpposite i) == opy = 20
 -- it's an open corner
 | unblocked && aCorner itsNexus  = 6
 -- it possess some other advantage ...
 | or $ map (> Playable) scoresMe = 4
 -- well, it isn't blocked at least
 | unblocked = 2
 -- we're on our way to a draw
 | otherwise = 0
  where itsNexus = nexus i
        scoredMe = map (scoreSqListFor ply) (rows i)
        scoresMe = map fst scoredMe
        scoredOp = map (scoreSqListFor opy) (rows i)
        scoresOp = map fst scoredOp
        unblocked = or $ map (> Blocked) scoresMe
        itsOpposite = opposite itsNexus  -- "opposite location"
        opy = otherPlayer ply
        itsNextTos = nextTosi i
        myNextTos = filter (\sq -> tic sq == ply) itsNextTos
        opNextTos = filter (\sq -> tic sq == opy) itsNextTos

-- index a square out of an intersection's "rows"
-- caller expected to know it will be there, so doesn't protect against empty list
squareAt :: Location -> Intersection -> Square
squareAt  loc i = head $ filter (\sq -> location sq == loc) sqs
  where sqs = concat (rows i)

-- "next to" == adjacent && (in a winning sequence) - e.g. diagonals are only "next to" corners or centre

allNextTos :: [Location] -> [Square] -> [(Location, [Square])]
allNextTos _ [] = []
allNextTos ls sqs =  [(loc, (nextTos sqs loc)) | loc <- ls]

-- retrieve a list of squares "next to" an intersection
nextTosi :: Intersection -> [Square]
nextTosi i = nextTos (concat $ rows i) (nexus i)

-- retireve a list of squares "next to" a location from supplied list of squares
nextTos :: [Square] -> Location -> [Square]
nextTos [] _ = []
nextTos sqs loc = filter (\sq -> nextTo loc (location sq)) sqs

-- are squares "next to"  one another?
nextToSq :: Square -> Square -> Bool
nextToSq sq1 sq2 = nextTo (location sq1) (location sq2)

-- are locations "next to"  one another?
nextTo :: Location -> Location -> Bool
nextTo l1 l2 = elem l2 (adjacentLocations l1)

-- perhaps these nextTo / adjacentCorners could be computed, but pattern match is easy

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
shareRow loc@(row,col) = diffs [l | l@(r,c) <- usableLocations, r == row] [loc]

shareColumn :: Location -> [Location]
shareColumn loc@(row,col) = diffs [l | l@(r,c) <- usableLocations, c == col] [loc]

shareDiag :: Location -> [Location]
shareDiag loc = []


--   for "rows" of squares ... logic makes no sense if this is a random collection of squares
scoreSqListFor :: Player -> [Square] -> (Score, [Square])
scoreSqListFor ply sqs
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
        players = ticCount ply sqs
        opponent = otherPlayer ply
        opponents = ticCount opponent sqs

-- / game strategy

-- / simple game strategy

smartMove :: Board -> Board
smartMove brd
  | isJust loc = makeMove brd (fromJust loc)
  | otherwise = brd
  where loc = pickUnplayedSquare $ head $ rankBoardRows brd (whosMove brd)

-- from a list of squares, return location of the 1st unticked one
-- using fairly simple ranking
pickUnplayedSquare :: [Square] -> Maybe Location
pickUnplayedSquare squares
  | length sqs == 0 = Nothing
  | otherwise = Just (location $ head $ rankSquares sqs)
  where sqs = filter isUnplayed squares


-- order "rows" by how 'good' they are for Player
rankBoardRows :: Board -> Player ->  [[Square]]
rankBoardRows brd ply = sortBy  (rankSqList ply) (playableRows brd)


-- by score ... descending (compare is backwards)
rankSqList :: Player -> [Square] -> [Square] -> Ordering
rankSqList ply first second
  | score1st > score2nd = LT
  | score1st < score2nd = GT
  | otherwise = EQ
  where score1st = fst $ scoreSqListFor ply first
        score2nd = fst $ scoreSqListFor ply second

--- order squares by "rank" descending
rankSquares :: [Square] -> [Square]
rankSquares squares = sortBy rankSquare squares

rankSquare :: Square -> Square -> Ordering
rankSquare sq1 sq2 = rankLocation (location sq1) (location sq2)

-- \ simple game strategy



-- / square state functions

-- weights a collection of "rows", by summing player's tics for those rows not occuped by opponent
ticCountSumUseful :: Player -> [[Square]] -> Int
ticCountSumUseful ply sqls = foldr (+) 0 (map (ticCount ply) (filter (isUnplayedFor (otherPlayer ply)) sqls))

ticCount :: Player -> [Square] -> Int
ticCount ply squares = length $ filter (\a -> tic a == ply) squares

isUnplayedFor :: Player -> [Square] -> Bool
isUnplayedFor ply squares = length (filter (\sq -> tic sq == ply) squares) == 0

hasUnplayed :: [Square] -> Bool
hasUnplayed squares = length (filter (\sq -> tic sq == N) squares) > 0

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

isUnplayedLocation :: Board -> Location -> Bool
isUnplayedLocation brd loc = elem loc $ unplayedLocations brd

allLocations :: Board -> [Location]
allLocations brd = map location $ squares brd

unplayedLocations :: Board -> [Location]
unplayedLocations brd = map location $ filter isUnplayed $ squares brd

playableRows :: Board -> [[Square]]
playableRows brd = filter hasUnplayed (winningRows brd)

winningRows :: Board -> [[Square]]
winningRows brd = map (squaresFor brd) winners

-- given a location & board, return location's winning combos from board
winningCombos :: Location -> Board -> [[Square]]
winningCombos theLocation brd = (filter (\w -> elem theLocation (map location w))) (winningRows brd)

unplayedIntersections :: Board -> [Intersection]
unplayedIntersections brd =  filter (\i -> isUnplayedLocation brd (nexus i)) (byIntersections brd)
  where byIntersections :: Board -> [Intersection]
        byIntersections  brd = map (\loc -> Intersection loc (winningCombos loc brd)) usableLocations

-- represent a board as a list of lists of tallys for each unplayed location
unplayedTallys :: Board -> [(Location, [[Tally]])]
unplayedTallys  brd = map asTally (unplayedIntersections brd)


byNextTos :: Board -> [(Location, [Square])]
byNextTos brd = allNextTos (map location sqs) sqs
  where sqs = squares brd

winner :: Board -> Player -> Bool
winner brd ply =  or $ map (\w -> isInfixOf w ticked) winners
  where ticked = map location (filter (\sq -> (tic sq) == ply) (squares brd))

whoWon :: Board -> Player
whoWon brd
 | winner brd X = X
 | winner brd O = O
 | otherwise = N

-- who won & how many moves it took
--  ('N',9) == a draw
--  ('N', [0..8]) == an unfinished game
boardOutcome :: Board -> Outcome
boardOutcome brd  = Outcome (whoWon brd) (movesCount brd)

aWinner :: Board -> Bool
aWinner brd = whoWon brd /= N

finished :: Board -> Bool
finished brd = (movesCount brd == 9) || (aWinner brd)

movesCount :: Board -> Int
movesCount brd = 9 - length (unplayedSquares brd)

movesList :: Board -> Board -> [Square]
movesList start finish =  sortByMove (diffBoards start finish)

-- give all moves made by winning player, not just winning sequence
howWon :: Board -> (Player, [Location])
howWon brd = (winner, map location sqForWinner)
  where winner = whoWon brd
        sqForWinner = [sq | sq <- (squares brd), tic sq == winner]

-- if board is empty, assumes 'x' plays first ...
whosMove :: Board -> Player
whosMove brd
 | movesLeft == 0 = N
 | mod movesLeft 2 == 0 = O
 | otherwise = X
 where movesLeft = length $ unplayedSquares brd

whichMove :: Board -> Move
whichMove brd = 10 - (length $ unplayedSquares brd)

boardMoves :: [Board] -> [Square]
boardMoves [] = []
boardMoves (bb:ba:bs) = (diffBoards ba bb) ++ boardMoves (ba:bs)
boardMoves (bb:bs)
 | length bs == 0 = []
 | otherwise = (diffBoards (head bs) bb)

diffBoards :: Board -> Board -> [Square]
diffBoards b1 b2 = diffSquares (squares b1) (squares b2)

diffSquares :: [Square] -> [Square] -> [Square]
diffSquares sqs1 sqs2 = DS.toList $ DS.difference  (DS.fromList sqs2) (DS.fromList sqs1)

unplayedSquares :: Board -> [Square]
unplayedSquares brd = filter (\sq -> tic sq == N) (squares brd)

playedSquares :: Board -> [Square]
playedSquares brd = filter (\sq -> tic sq /= N) (squares brd)



-- \ board state functions

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

-- / programmed play ... useful for testing

-- play a supplied sequence of moves, alternating players
playMoves :: [Location] -> Board
playMoves ps =  snd $ moveThrough (ps, newBoard)


playAllGamesFrom :: Int -> [Board]
playAllGamesFrom idx =
 playGame idx : playAllGamesFrom (idx+1)

-- play pre-defined game # n, return the board when game ends
-- 362,880 possible game sequences (although there's only 26,830 distinct games)
playGame :: Int -> Board
playGame n =  snd $ moveThrough (game, newBoard)
  where game =  allPlaySequences !! mod n 362880

allPlaySequences = permu usableLocations

-- given a sequence of locations & a board,
-- play until someone wins or no more moves
moveThrough :: ([Location], Board) -> ([Location], Board)
moveThrough (ls, brd)
  | length ls == 0 = (ls, brd)
  | aWinner brd = (ls, brd)
  | otherwise = moveThrough (tail ls, (makeSuppliedMove brd (head ls)))

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

permu :: [a] -> [[a]]
permu [] = [[]]
permu [x] = [[x]]
permu (x:xs) = concat (map (interl x) (permu xs))

interl :: a -> [a] -> [[a]]
interl a [] = [[a]]
interl a (x: xs) =
  (a:x:xs) : map (x:) (interl a xs)

-- TODO remove when no longer needed
-- load some test data ... for ghci devel
apg = allPossibleGames cleverMove newBoard
apo = map gameOutcome apg
apgo = zip apg apo

apgX = gamesFor apg X
apgO = gamesFor apg O
apgN = gamesFor apg N

apgoO = [g | (g,o@Outcome{player=ply,moves=m}) <- apgo, ply == O]
apgoX = [g | (g,o@Outcome{player=ply,moves=m}) <- apgo, ply == X]
apgoN = [g | (g,o@Outcome{player=ply,moves=m}) <- apgo, ply == N]

gO0 = head apgoO
gX0 = head apgoX
gN0 = head apgoN

gX = head apgX
brd = fromJust $ boardForMove gX 3
ply = whosMove brd
opy = otherPlayer ply
forceable = canForce ply brd
forkableByOpponent = canFork opy brd
inboth = intersect forceable forkableByOpponent
forceableOnly = diffs forceable forkableByOpponent
blockables = blocking brd



gamesFor :: [Game] -> Player -> [Game]
gamesFor games py = [g | g@Game{boards=bds} <- games, (player $ gameOutcome g) == py]
