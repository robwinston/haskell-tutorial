-- from https://www.youtube.com/watch?v=3bjXGrycMhQ
-- A History of Haskell: being lazy with class @ 19:22

module Main where

main =  do {
  putStrLn "I just write this string to console ..."
}

echo :: IO ()
echo = getChar >>= putChar

asBinds = getChar >>= (\a -> getChar >>= (\b -> putChar b >>= \() -> return (a,b)))


-- do-notation is syntactic sugar, with a deliberately imperative "look and feel"
asDo = do {
  a <- getChar;
  b <- getChar;
  putChar b;
  return (a,b)
}

-- just verifying correct syntaxe without the putChar  ...
asBinds2 = getChar >>= (\a -> getChar >>= (\b -> return (a,b)))


-- defining one's own "control structures" ...
forever :: IO () -> IO ()
forever a = do { a; forever a }

-- forever $ putChar 'x'  ... will indeed!

repeatN :: Int -> IO () -> IO ()
repeatN 0 a = return ()
repeatN n a = do { a; repeatN (n-1) a }

-- repeatN 10 $ putChar 'x'

