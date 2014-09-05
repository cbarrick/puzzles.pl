:- use_module(library(clpfd)).


%% matrix/3
% matrix(?N, ?M, ?M)
% True if M is a N x M matrix.
matrix(1, M, [Row]) :- length(Row, M).
matrix(N, M, [Row|Rows]) :-
	N #> 1,
	N0 #= N-1,
	length(Row, M),
	matrix(N0, M, Rows),
	!.


%% nonogram/3
% nonogram(+Rows, +Cols, -Solution)
% Solves a nonogram where Rows is the list of row clues
% and Cols is the list of column clues.
nonogram(Rows, Cols, Solution) :-
	length(Rows, NRows),
	length(Cols, NCols),
	matrix(NCols, NRows, Solution),
	transpose(Solution, TransSolution),
	maplist(nonogram_1d, Rows, Solution),
	maplist(nonogram_1d, Cols, TransSolution).


%% nonogram_1d/2
% nonogram(?Clues, ?List)
% True if List is a row or column of a nonogram constrained by the Clues and
% marked with 'X' and '_' for full and blank cells respectively.
%
% More precisely, Clues is a list of integers > 0 and List is a list of 'X's
% and '_'s such that, for each clue, List contains a contiguous range of 'X's
% of the length of the clue. The ranges do not touch and are in the same order
% as the Clues list. All other elements of List are '_'.
%
% Equivalent to nonogram_1d/3 where N < 0.
nonogram_1d(Clues, List) :-
	nonogram_1d(Clues, List, -1).

%% nonogram_1d/3
% nonogram(?Clues, ?List, +N)
% Same as nonogram_1d/2 with the additional constraint that List begins
% with a range of 'X's of length N. If N is 0, List starts with a '_'.
% If N < 0, no assertion is made about the first element.

% Base case
% You are allowed to say an empty list starts with a blank cell (N = 0),
% but you cannot say an empty list starts with a full cell (N > 0).
nonogram_1d([], [], N) :- N < 1.

% If N = -1, either the first range starts at the head of List...
nonogram_1d([Clue|Clues], ['X'|List], -1) :-
	nonogram_1d(Clues, ['X'|List], Clue).

% ... or the list starts with a blank
nonogram_1d(Clues, ['_'|List], -1) :-
	nonogram_1d(Clues, List, -1).

% If N = 0, List starts with '_'
nonogram_1d(Clues, ['_'|List], 0) :-
	nonogram_1d(Clues, List, -1).

% If N > 0, List starts with a range of 'X's
nonogram_1d(Clues, ['X'|List], N) :-
	N > 0,
	N_Next is N - 1,
	nonogram_1d(Clues, List, N_Next).


% End of library, test code below
% -------------------------

%% nonogram_writeMatrix/1
% nonogram_writeMatrix(M)
% Writes the matrix M.
nonogram_writeMatrix(Rows) :-
	write('['),
	nl,
	nonogram_writeMatrix_doWrite(Rows),
	write(']'),
	nl.
nonogram_writeMatrix_doWrite([Row|Rows]) :-
	write('  '),
	write(Row),
	(Rows \= []) -> (
		write(','),
		nl,
		nonogram_writeMatrix_doWrite(Rows)
	) ; (
		nl
	).

%% nonogram_test_small/0
% Solves a 7x7 nonogram
nonogram_test_small :-
	nonogram(
		[
			[2,2],
			[7],
			[2,4],
			[7],
			[5],
			[3],
			[1]
		],
		[
			[3],
			[5],
			[2,3],
			[6],
			[6],
			[5],
			[3]
		],
		Solution
	),
	nonogram_writeMatrix(Solution), nl,
	!.

%% nonogram_test_big/0
% Solves a 20x20 nonogram
nonogram_test_big :-
	write('Solving a 20x20 nonogram'), nl,
	write('This will take a while'), nl,
	nonogram(
		[
			[3],
			[5],
			[3,1],
			[2,1],
			[3,3,4],
			[2,2,7],
			[6,1,1],
			[4,2,2],
			[1,1],
			[3,1],
			[6],
			[2,7],
			[6,3,1],
			[1,2,2,1,1],
			[4,1,1,3],
			[4,2,2],
			[3,3,1],
			[3,3],
			[3],
			[2,1]
		],
		[
			[2],
			[1,2],
			[2,3],
			[2,3],
			[3,1,1],
			[2,1,1],
			[1,1,1,2,2],
			[1,1,3,1,3],
			[2,6,4],
			[3,3,9,1],
			[5,3,2],
			[3,1,2,2],
			[2,1,7],
			[3,3,2],
			[2,4],
			[2,1,2],
			[2,2,1],
			[2,2],
			[1],
			[1]
		],
		Solution
	),
	nonogram_writeMatrix(Solution), nl,
	!.
