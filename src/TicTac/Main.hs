module Main where

import TicTac.Play
import TicTac.Simple
import TicTac.Common

import Text.Read
import Data.Maybe
import Data.Char

main =
  do
    games <- playGames []
    displaySummary $ reverse games


playGames :: [GameInfo] -> IO [GameInfo]
playGames games =
  do
    py <- getPlayer
    if (elem py actualPlayers)
      then
        do
         putStrLn ("Playing as: " ++ show py)
         gameInfo <- playGameAs py
         print . last . boards $ game gameInfo
         playGames (gameInfo : games)
      else
        do
         return games

displaySummary :: [GameInfo]  -> IO ()
displaySummary gifs =
  do
    if gss == []
      then putStrLn "No games played"
      else putStrLn summary
      where gss = gamesSummary gifs
            summary = foldr (++) "" (map (\(i,gs) -> "\n" ++ show i ++ ": " ++ gs) (zip [1..] gss))




playGameAs :: Player -> IO GameInfo
playGameAs ply
  | ply == X = do
                brds <- interactivePlay [newBoard]
                return $ aGameInfoFrom (aGameFrom brds) ply
  | ply == O = do
                brds <- interactivePlay $ (cleverMove newBoard) : [newBoard]
                return $ aGameInfoFrom (aGameFrom brds) ply




interactivePlay :: [Board] -> IO [Board]
interactivePlay [] = return []
interactivePlay (brd:brds)
  | finished brd = return (brd:brds)
  | otherwise = do
    putStr $ show brd
    mv <- getMove brd
    if isNothing mv
      then return (brd:brds)
      else interactivePlay ((playARoundUsingL cleverMove brd (fromJust mv)) : (brd:brds))

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
