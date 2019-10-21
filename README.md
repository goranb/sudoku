# λearning Sudoku

An experiment in making a sudoku generator in Haskell, to learn me a Haskell for greater good.

This is the current output:

```
┏━━━┯━━━┯━━━┳━━━┯━━━┯━━━┳━━━┯━━━┯━━━┓
┃ 7 │ 1 │ 8 ┃ 2 │ 5 │ 3 ┃ 9 │ 6 │ 4 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 2 │ 5 │ 9 ┃ 4 │ 6 │ 1 ┃ 7 │ 3 │ 8 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 3 │ 4 │ 6 ┃ 8 │ 9 │ 7 ┃ 2 │ 1 │ 5 ┃
┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫
┃ 9 │ 3 │ 1 ┃ 7 │ 8 │ 4 ┃ 5 │ 2 │ 6 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 8 │ 6 │ 2 ┃ 3 │ 1 │ 5 ┃ 4 │ 9 │ 7 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 4 │ 7 │ 5 ┃ 6 │ 2 │ 9 ┃ 1 │ 8 │ 3 ┃
┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫
┃ 5 │ 2 │ 3 ┃ 1 │ 4 │ 8 ┃ 6 │ 7 │ 9 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 6 │ 9 │ 7 ┃ 5 │ 3 │ 2 ┃ 8 │ 4 │ 1 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 1 │ 8 │ 4 ┃ 9 │ 7 │ 6 ┃ 3 │ 5 │ 2 ┃
┗━━━┷━━━┷━━━┻━━━┷━━━┷━━━┻━━━┷━━━┷━━━┛
```

It accepts 2 parameters, `format` and `padding`:
- `padding` is just a number. Anything will do, as long as it's 0+ (defaults to `1`)
- `format` is a string (`lines`, `plain`, or `oneliner`) that sets the output format. Lines is above.

The padding will be applied horizontally (and half vertically) to each cell.

This is `sudoku plain 0`:

```
768425139
231976584
549138276
386519427
452687391
197342658
923864715
615793842
874251963
```

This is `sudoku oneliner` (the padding is ignored):

```
247615398963482751851793426125874639739156842486239175614927583398541267572368914
```

This is my first "real" Haskell program, BTW. :)


## Progress

So far the logic for random solved sudoku setup is working. It's utilizing a stream of random numbers, and it tries to conform them to the grid, according to the rule that the digit needs to be unique in the row, the column, and the 3x3 block section. In case it gets stuck, it starts over again until it finds an answer.

Next will be the "well-formed" sudoku format, that guarantees a single solution. It will take me some additional research to get to that. Once that is completed, the baseline will be accomplished.

Future:
- optional show/hide RNG seed to be able to reproduce the puzzle
- offer solution print (together or separately)
- different layouts (CSV, double-piped box-drawing characters, PDF?,...)
