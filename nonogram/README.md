# [Nonogram](http://en.wikipedia.org/wiki/Nonogram)

Nonograms are logic puzzles describing binary images.


# nonogram/3

`nonogram(+Rows, +Cols, ?Solution)`

Solves a nonogram where Rows is the list of row clues and Cols is the list of column clues.


# nonogram/4

`nonogram(+Rows, +Cols, ?Solution, +Solver)`

Solves a nonogram using the given Solver. A solver is an implementation of the 1-demensional case. This library provides the following solvers:

- `nonogram_1d/2` is a clpfd wrapper around `nonogram_1d_naive`. Good performance.
- `nonogram_1d_naive/2` uses backtracking. Slow!
- `nonogram_1d_automaton/2` uses clpfd to create an NFA that constrains each row/column. Fastest.


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
  [0, 1, 1, 0, 1, 1, 0],
  [1, 1, 1, 1, 1, 1, 1],
  [1, 1, 0, 1, 1, 1, 1],
  [1, 1, 1, 1, 1, 1, 1],
  [0, 1, 1, 1, 1, 1, 0],
  [0, 0, 1, 1, 1, 0, 0],
  [0, 0, 0, 1, 0, 0, 0]
]
```


# Testing and Benchmarks

This library includes a bunch of predicates for testing and benchmarking nonogram implementations. Use `nonogram_test/0` to compare the included solvers. Use `nonogram_test/1` to see statistics for any specific solver on various test puzzles. The puzzles can also be tested individually:

- `nonogram_test_hen(Solver)` is a 9x8 puzzle.
- `nonogram_test_boat(Solver)` is a 10x20 puzzle.
- `nonogram_test_beanstalk(Solver)` is a 25x20 puzzle.
- `nonogram_test_waterBuffalo(Solver)` is a 20x25 puzzle.
- `nonogram_test_soccer(Solver)` is a 20x20 puzzle.
- `nonogram_test_ambiguous(Solver)` is a 4x4 puzzle with 3 solutions.
