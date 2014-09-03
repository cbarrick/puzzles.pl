# [Sudoku](http://en.wikipedia.org/wiki/Sudoku)

Sudoku is a simple number-placement puzzle.


## sudoku/1

`sudoku(Rows)`

True if the list of Rows of the puzzle constitutes a sudoku solution.

`Rows` is the 9x9 matrix of the sudoku board, indexed by row then column.


## Example

Solves the puzzle on the sudoku Wikipedia page:

```prolog
?- Rows = [
	[5, 3, _,   _, 7, _,   _, _, _],
	[6, _, _,   1, 9, 5,   _, _, _],
	[_, 9, 8,   _, _, _,   _, 6, _],

	[8, _, _,   _, 6, _,   _, _, 3],
	[4, _, _,   8, _, 3,   _, _, 1],
	[7, _, _,   _, 2, _,   _, _, 6],

	[_, 6, _,   _, _, _,   2, 8, _],
	[_, _, _,   4, 1, 9,   _, _, 5],
	[_, _, _,   _, 8, _,   _, 7, 9]
], sudoku(Rows).

Rows = [
	[5, 3, 4, 6, 7, 8, 9, 1, 2],
	[6, 7, 2, 1, 9, 5, 3, 4, 8],
	[1, 9, 8, 3, 4, 2, 5, 6, 7],
	[8, 5, 9, 7, 6, 1, 4, 2, 3],
	[4, 2, 6, 8, 5, 3, 7, 9, 1],
	[7, 1, 3, 9, 2, 4, 8, 5, 6],
	[9, 6, 1, 5, 3, 7, 2, 8, 4],
	[2, 8, 7, 4, 1, 9, 6, 3, 5],
	[3, 4, 5, 2, 8, 6, 1, 7, 9]
].
```
