:- use_module(library(clpfd)).


sudoku(Rows) :-
	sudoku_getBlocks(Rows, Blocks),
	transpose(Rows, Cols),
	maplist(sudoku_valid, Rows),
	maplist(sudoku_valid, Cols),
	maplist(sudoku_valid, Blocks),
	maplist(label, Rows).

sudoku_valid(List) :-
	List ins 1..9,
	all_distinct(List).

sudoku_getBlocks(Rows, Blocks) :-
	Rows = [
		[A1, A2, A3,   B1, B2, B3,   C1, C2, C3],
		[A4, A5, A6,   B4, B5, B6,   C4, C5, C6],
		[A7, A8, A9,   B7, B8, B9,   C7, C8, C9],

		[D1, D2, D3,   E1, E2, E3,   F1, F2, F3],
		[D4, D5, D6,   E4, E5, E6,   F4, F5, F6],
		[D7, D8, D9,   E7, E8, E9,   F7, F8, F9],

		[G1, G2, G3,   H1, H2, H3,   I1, I2, I3],
		[G4, G5, G6,   H4, H5, H6,   I4, I5, I6],
		[G7, G8, G9,   H7, H8, H9,   I7, I8, I9]
	],
	Blocks = [
		[A1, A2, A3, A4, A5, A6, A7, A8, A9],
		[B1, B2, B3, B4, B5, B6, B7, B8, B9],
		[C1, C2, C3, C4, C5, C6, C7, C8, C9],
		[D1, D2, D3, D4, D5, D6, D7, D8, D9],
		[E1, E2, E3, E4, E5, E6, E7, E8, E9],
		[F1, F2, F3, F4, F5, F6, F7, F8, F9],
		[G1, G2, G3, G4, G5, G6, G7, G8, G9],
		[H1, H2, H3, H4, H5, H6, H7, H8, H9],
		[I1, I2, I3, I4, I5, I6, I7, I8, I9]
	].

sudoku_test(Rows) :-
	Rows = [
		[5, 3, _,   _, 7, _,   _, _, _],
		[6, _, _,   1, 9, 5,   _, _, _],
		[_, 9, 8,   _, _, _,   _, 6, _],

		[8, _, _,   _, 6, _,   _, _, 3],
		[4, _, _,   8, _, 3,   _, _, 1],
		[7, _, _,   _, 2, _,   _, _, 6],

		[_, 6, _,   _, _, _,   2, 8, _],
		[_, _, _,   4, 1, 9,   _, _, 5],
		[_, _, _,   _, 8, _,   _, 7, 9]
	],
	sudoku(Rows).
