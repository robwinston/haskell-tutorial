module Hearts.Main where

import System.Random
import Data.List
import Hearts.Cards
import Hearts.Common


main =
  do
   gen <- newStdGen
   let cards = shuffle gen fullDeck
   putStrLn $ (show $ head cards) ++ " .. " ++ (show $ last cards)