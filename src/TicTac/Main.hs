module Main where

import TicTac.Play
import TicTac.Simple
import TicTac.Common

import Text.Read
import Data.Maybe
import Data.Char

main = do
         py <- getPlayer
         if (elem py actualPlayers)
           then
             do
              putStrLn ("Playing as: " ++ show py)
              brds <- playGameAs py
              putStrLn ((show $ aGameFrom brds) ++ "\nNice playing you")
           else
             do
              putStrLn "TTFN!"

playGameAs :: Player -> IO [Board]
playGameAs ply
  | ply == X = do
                brd <- interactivePlay [newBoard]
                return brd
  | ply == O = do
                brd <- interactivePlay $ (cleverMove newBoard) : [newBoard]
                return brd


interactivePlay :: [Board] -> IO [Board]
interactivePlay brds = return brds

getPlayer :: IO Player
getPlayer = go ("Who to play as? " ++ (show players)  ++ " -- N to quit")
  where go prompt = do
              putStrLn prompt
              input <- getLine
              let ply = readMaybe input :: Maybe Player
              if isJust ply
                then return $ fromJust ply
                else go ("Please select one of: " ++ (show players)  ++ " -- N to quit")

getMove :: Board -> IO (Maybe Location)
getMove brd =  if null $ upl
     then
      return Nothing
     else
      go
        where go = do
                     putStrLn  ("Next move? " ++ show upl)
                     input <- getLine
                     let loc = readMaybe input :: Maybe Location
                     if isJust loc  && elem (fromJust loc) upl
                       then return loc
                       else go
              upl = unplayedLocations brd
