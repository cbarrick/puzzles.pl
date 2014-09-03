:- use_module(library(clpfd)).

kakuro(Ranges, Sums) :-
	kakuro_all(Ranges, Sums),
	!.

kakuro_all(Ranges, Sums) :-
	maplist(kakuro_1D, Ranges, Sums),
	maplist(label, Ranges).

kakuro_1D(Range, Sum) :-
	Range ins 1..9,
	sum(Range, #=, Sum),
	all_distinct(Range).


kakuro_test(L) :-
	% http://www.atksolutions.com/games/kakuro.gif
	L = [
		[A, B],
		[C, D, E],
		   [F, G, H],
		[A, C],
		[B, D, F],
		[E, G]
	],
	Sums = [3, 17, 18, 4, 9, 17],
	kakuro(L, Sums).


kakuro_test2(List) :-
	% http://upload.wikimedia.org/wikipedia/commons/7/72/Kakuro_black_box_solution.svg
	List = [
		[A,  B],        [C,  D,  E],
		[F,  G],    [H,  I,  J,  K],
		[L,  M,  N,  O,  P],
			[Q,  R],    [S,  T],
				[U,  V,  W,  X,  Y],
		[Z,  AA, BB, CC],   [DD, EE],
		[FF, GG, HH],       [II, JJ],

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
	],
	Sums = [16, 24, 17, 29, 35, 7,  8, 16, 21, 5,  6,  3,
	        23, 11, 30, 10, 15, 17, 7, 27, 12, 12, 16, 7],
	kakuro_all(List, Sums).
