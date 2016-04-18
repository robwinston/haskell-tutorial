# haskell-tutorial
Me learning haskell, so bear this in mind if reviewing code.  Code will evolve as learning curve is scaled.

## TicTac.hs
Primitive Tic-Tac-Toe (Naughts & Crosses), playable from ghci prompt.

There are four ways to play it.  All *play* functions share common behaviour:

* Return the updated board - you must capture this to perform successive plays
* Use `newBoard` to get an unplayed board
* Board *position* ranges from 1-9 in row-major order
* If *position* supplied to function is <1 or >9, computer will make next move
* If *position* supplied is already marked, board remains unchanged
* *strategy* is a `Board -> Board` function (see code for what a `Board` is
* There are two pre-defined strategies: `smartMove` and `smarterMove` (it remains to be seen if they're appropriately named)
* Of course *strategy* is used only when the computer is playing

Play functions:

1. `play board position`  - make next move at requested position - (employs the `smarterMove` strategy if computer moves)
2. `playUsing strategy board position` - make next move at requested position - (employs the specified strategy if computer moves)
3. `playARound board position`  - make next move at requested position, then make next move automatically 
4. `playARoundUsing strategy board position` - make next move at requested position, then make next move automatically

Note: if the `playARound` functions are invoked supplying an already used position, the behaviour will be the same as `play` supplying an out-of-range position - this is becasue the 1st play will do nothing, then the computer will play - in effect playing for you. 


There are also some *auto-play* functions:

1. `autoPlay board` - computer plays the supplied board until there's a winner or a draw using the `smarterMove` strategy
2. `autoPlayFrom position` - computer starts a new board, plays the requested position, then carries on like `autoPlay`
3. `autoPlayUsing ` & `autoPlayFromUsing` are the variations which takea supplied *strategy* 


The `smartMove` is strategy is easily defeated (so not *very* smart after all); the `smarterMove` strategy can be beat, but less easily. 

[Sample Play](SAMPLEGAME.md)