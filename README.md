# haskell-tutorial
Me learning haskell, so bear this in mind if reviewing code.  Code will evolve as learning curve is scaled.

## TicTac.hs
Primitive Tic-Tac-Toe (Naughts & Crosses), playable from ghci prompt.

There are four ways to play it.  All *play* functions share common behaviour:

* Return the updated board - you must capture this to perform successive plays
* Use `newBoard` to get an unplayed board
* Board *Location* range is `[TLC,TM,TRC,LM,CTR,RM,BLC,BM,BRC]`; there is also a `NOL` location as a no-op.
* If *Location* supplied to function is `NOL`, computer will make next move
* If *Location* supplied is already marked, board remains unchanged
* *strategy* is a `Board -> Board` function (see code for what a `Board` is
* There are two pre-defined strategies: `smartMove` and `smarterMove` The `smartMove` strategy is easily defeated (so not *very* smart after all); the `smarterMove` strategy can possibly be beat, but less easily. 

* Of course *strategy* is used only when the computer is playing

Play functions:

1. `play Board Location`  - make next move at requested Location - (employs the `smarterMove` strategy if computer moves)
2. `playUsing Strategy Board Location` - make next move at requested Location - (employs the specified strategy if computer moves)
3. `playARound Board Location`  - make next move at requested Location, then make next move automatically 
4. `playARoundUsing Strategy Board Location` - make next move at requested Location, then make next move automatically

Note: if the `playARound` functions are invoked supplying an already used Location, the behaviour will be the same as `play` supplying an out-of-range Location - this is becasue the 1st play will do nothing, then the computer will play - in effect playing for you. 


There are also some *auto-play* functions:

1. `autoPlay Board` - computer plays the supplied board until there's a winner or a draw using the `smarterMove` strategy
2. `autoPlayFrom Location` - computer starts a new board, plays the requested Location, then carries on like `autoPlay`
3. `autoPlayUsing ` & `autoPlayFromUsing` are the variations which take a supplied *strategy* 



[Sample Play](SAMPLEGAME.md)