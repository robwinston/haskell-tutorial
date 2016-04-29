module Hearts.Common where

import qualified Data.Map as DM
import Hearts.Cards

newtype SCard = SCard { card :: Card }
 deriving (Eq, Show)

instance Ord SCard
  where compare c1@(SCard((p1,s1))) c2@(SCard((p2,s2)))
          | s1 < s2 = LT
          | s1 > s2 = GT
          | p1 < p2 = LT
          | p1 > p2 = GT
          | otherwise = EQ


type Hand = [Card]