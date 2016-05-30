module Main where

c = putStrLn "C!"

{-
ghci> :t combine
combine :: IO a -> IO b -> IO b
-}
combine before after =
  do before
     putStrLn "In the middle"
     after

{-
ghci> :t main
main :: IO ()
-}
main = do combine c c
          let b = combine (putStrLn "Hello!") (putStrLn "Bye!")
          let d = combine (b) (combine c c)
          putStrLn "So long!"

