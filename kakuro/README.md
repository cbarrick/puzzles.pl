# [Kakuro](http://en.wikipedia.org/wiki/Kakuro)

A Kakuro is a "cross-sum" puzzle.


## kakuro/2

`kakuro(Ranges, Sums)`

True if `Ranges` is a list of lists of variables and `Sums` is a list of positive integers such that each list in `Ranges` sums to the corresponding integer in `Sums` and each list contains distinct integers between 1 and 9.

Since kakuro puzzles officially have only one solution, the `kakuro/2` predicate is cut. `kakuro_all/2` is the uncut equivalent.


## Example

Solves the puzzle on the kakuro Wikipedia page:

```prolog
?- kakuro_all([
		% Horizontal ranges
		[A,  B],        [C,  D,  E],
		[F,  G],    [H,  I,  J,  K],
		[L,  M,  N,  O,  P],
			[Q,  R],    [S,  T],
				[U,  V,  W,  X,  Y],
		[Z,  AA, BB, CC],   [DD, EE],
		[FF, GG, HH],       [II, JJ],

		% Vertical ranges
		[A, F, L],
		[Z, FF],
		[B, G, M, Q],
		[AA, GG],
		[N, R, U, BB, HH],
		[H, O],
		[V, CC],
		[C, I, P, S, W],
		[D, J],
		[T, X, DD, II],
		[E, K],
		[Y, EE, JJ]
	], [
		% Sums
		16, 24, 17, 29, 35, 7,  8, 16, 21, 5,  6,  3,
		23, 11, 30, 10, 15, 17, 7, 27, 12, 12, 16, 7
	]).
```
