module Main where

import TicTac.Play
import TicTac.Simple
import TicTac.Common

main = do
    putStrLn "Who to play as? [X,O]"
    role <- getLine
    putStrLn ("Playing as " ++ role )