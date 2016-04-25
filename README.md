# haskell-tutorial
Me learning haskell, so bear this in mind if reviewing code.  Code will evolve as learning curve is scaled.

## TicTac.hs
Primitive Tic-Tac-Toe (Naughts & Crosses), playable from ghci prompt.

There are four ways to play it.  All *play* functions share common behaviour:

* Return the updated board - you must capture this to perform successive plays
* Use `newBoard` to get an unplayed board
* Board *location* range is a value from 1-9, expressed in row major order
* If *location* supplied to function is out of range, computer will make next move
* If *location* supplied is already marked, board remains unchanged
* *strategy* is a `Board -> Board` function (see code for what a `Board` is
* There are three pre-defined strategies: `smartMove`, `smarterMove`, and `cleverMove`. The `smartMove` strategy is easily defeated (so not *very* smart after all); the `smarterMove` strategy is also defeatable, but somewhat less easily. If the  `strategyChecker` (in TicTacTest.hs) is correctly implemented, it concludes that `cleverMove` is unbeatable, irrespective of who plays first. 


* Of course *strategy* is used only when the computer is playing

Basic play:

`play Board location`  - make next move using supplied board and at requested Location - (employs the `cleverMove` strategy if computer moves)

Test play (in TicTacTest.hs):

For these functions, board location expressed using the underlying `Location` representation of `(Row,Column)`, where `Row` is `T`, `M`, or `B`; `Column` is `L`, `R`, or `C`

1. `playUsing Strategy Board Location` - make next move at requested location - (employs the specified strategy if computer moves)
2. `playARoundUsing Strategy Board Location` - make next move at requested location using suppled strategy, then make next move automatically

Note: if the `playARound` functions are invoked supplying an already used Location, the behaviour will be the same as `play` supplying an out-of-range Location - this is becasue the 1st play will do nothing, then the computer will play - in effect playing for you. 


There are also some *auto-play* functions:

1. `autoPlayUsing Board` - computer plays the supplied board until there's a winner or a draw using the supplied strategy
2. `autoPlayFromUsing Location` - computer starts a new board, plays the requested Location, then carries on like `autoPlayUsing`


