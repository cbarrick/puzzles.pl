# [Nonogram](http://en.wikipedia.org/wiki/Nonogram)

Nonograms are logic puzzles describing binary images.


# nonogram/3

`nonogram(+Rows, +Cols, -Solution)`

Solves a nonogram where Rows is the list of row clues and Cols is the list of column clues.


# Example

Solves the nonogram:

![Example nonogram](http://www.griddler.co.uk/nonogram%20example.jpg)

```prolog
?- nonogram(
	[[2,2],[7],[2,4],[7],[5],[3],[1]],
	[[3],[5],[2,3],[6],[6],[5],[3]],
	Solution
).

Solution = [
  [_,X,X,_,X,X,_],
  [X,X,X,X,X,X,X],
  [X,X,_,X,X,X,X],
  [X,X,X,X,X,X,X],
  [_,X,X,X,X,X,_],
  [_,_,X,X,X,_,_],
  [_,_,_,X,_,_,_]
]
```
