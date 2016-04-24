module TicTacPark where

import TicTacCore


boardMoves :: [Board] -> [Square]
boardMoves [] = []
boardMoves (bb:ba:bs) = (diffBoards ba bb) ++ boardMoves (ba:bs)
boardMoves (bb:bs)
 | length bs == 0 = []
 | otherwise = (diffBoards (head bs) bb)


-- for a list of Locations & its Tallys, a Player, and a "count" predicate
--  > return Locations where there's at least one Tally meeting the criterion
locationsForWhere :: [(Location, [[Tally]] )] -> Player -> (Int -> Bool) -> [Location]
locationsForWhere tys ply prd =
    map fst  (filter (\(loc,t) -> t > 0) (map (\(loc,ts) -> (loc, length $ filter (\t -> prd $ countForPlayer ply t) ts)) tys))

byNextTos :: Board -> [(Location, [Square])]
byNextTos brd = allNextTos (map location sqs) sqs
  where sqs = squares brd

