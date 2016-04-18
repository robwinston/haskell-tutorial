```
→ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> :l TicTac.hs 
[1 of 2] Compiling MyLists          ( MyLists.hs, interpreted )
[2 of 2] Compiling TicTac           ( TicTac.hs, interpreted )
Ok, modules loaded: TicTac, MyLists.
# Start a new game & tic lower-right corner
ghci> let b1 = play newBoard 9
ghci> b1
"|N||N||N|"
"|N||N||N|"
"|N||N||X|"
O to move
# Make another move, in centre
ghci> let b2 = play b1 5
ghci> b2
"|N||N||N|"
"|N||O||N|"
"|N||N||X|"
X to move
# Make two successive moves -
# 1st move: upper-right corner; 2nd move: computer decides
ghci> let b3 = playARound b2 3
ghci> b3
"|N||N||X|"
"|N||O||O|"
"|N||N||X|"
X to move
# Make two successive moves - computer decides both
ghci> let b4 = playARound b3 0
ghci> b4
"|O||N||X|"
"|X||O||O|"
"|N||N||X|"
X to move
# Finish the game  automatically
ghci> let b5 = autoPlay b4
ghci> b5
"|O||X||X|"
"|X||O||O|"
"|X||O||X|"
It's a draw
ghci> 

```