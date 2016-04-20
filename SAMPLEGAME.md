```
→ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> :l TicTac.hs 
[1 of 2] Compiling MyLists          ( MyLists.hs, interpreted )
[2 of 2] Compiling TicTac           ( TicTac.hs, interpreted )
Ok, modules loaded: TicTac, MyLists.
ghci> autoPlayAllUsing smartMove 
[N,N,O,N,X,N,N,N,N]
ghci> autoPlayAllUsing smarterMove 
[N,N,N,N,N,N,N,N,N]
ghci> let b1 = autoPlay newBoard 
ghci> b1
|X|X|O|
|O|X|X|
|X|O|O|
It's a draw

ghci> let b1 = autoPlayFrom BRC
ghci> b1
|X|X|O|
|O|O|X|
|X|O|X|
It's a draw

# play interactively - playing both sides
ghci> let b1 = play newBoard TRC
ghci> b1
|N|N|X|
|N|N|N|
|N|N|N|
O to move

ghci> let b2 = play b1 BLC
ghci> b2
|N|N|X|
|N|N|N|
|O|N|N|
X to move

# let computer finish the game
ghci> let b3 = autoPlay b2
ghci> b3
|X|O|X|
|O|X|X|
|O|X|O|
It's a draw


# play interactively - against the computer
ghci> let b1 = playARound newBoard CTR
ghci> b1
|N|N|N|
|N|X|N|
|N|N|O|
X to move

ghci> let b2 = playARound b1 TRC
ghci> b2
|N|N|X|
|N|X|N|
|O|N|O|
X to move

ghci> let b3 = playARound b2 BM
ghci> b3
|N|O|X|
|N|X|N|
|O|X|O|
X to move

# let computer finish the game
ghci> let b4 = autoPlay b3
ghci> b4
|X|O|X|
|O|X|X|
|O|X|O|
It's a draw


# Also possible to create a 'Game' from a list of boards 
ghci> let b1 = autoPlayFrom TM
ghci> let g1 = Game [newBoard,b1]
ghci> g1
Game sequence: 
|N|N|N|
|N|N|N|
|N|N|N|
X to move

|X|X|O|
|O|O|X|
|X|O|X|
It's a draw

9 moves made
Moves: [|TM:X:1|,|CTR:O:2|,|BRC:X:3|,|TRC:O:4|,|BLC:X:5|,|BM:O:6|,|RM:X:7|,|LM:O:8|,|TLC:X:9|]
ghci> 


```