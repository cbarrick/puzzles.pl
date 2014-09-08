:- use_module(library(clpfd)).


% Helper Predicates
% ==================================================

%% matrix/3
% matrix(?N, ?M, ?Rows)
% True if Rows is a N x M matrix.
matrix(1, M, [Row]) :- length(Row, M).
matrix(N, M, [Row|Rows]) :-
	N #> 1,
	N #= N0 + 1,
	length(Row, M),
	matrix(N0, M, Rows),
	!.


% Nonogram Solver
% ==================================================

%% nonogram/3
% nonogram(+RowSpecs, +ColSpecs, -Solution)
% Solves a nonogram where Rows is the list of row clues
% and Cols is the list of column clues.
nonogram(RowSpecs, ColSpecs, Solution) :-
	length(RowSpecs, NRows),
	length(ColSpecs, NCols),
	matrix(NCols, NRows, Solution),
	transpose(Solution, SolutionT),

	% Apply the 1-demensional case to each row and column. There are 3 implementations:
	% `nonogram_1d/2` is a clfpd wrapper around `nonogram_1d_generator`. Good performance.
	% `nonogram_1d_generator/2` uses backtracking. Slow!
	% `nonogram_1d_automaton/2` uses clfpd to create an automaton to constrain each row. Fastest.
	maplist(nonogram_1d, RowSpecs, Solution),
	maplist(nonogram_1d, ColSpecs, SolutionT).


% nonogram_1d
% --------------------------------------------------

%% nonogram_1d/2
% nonogram_1d(+Clues, ?List)
% True if List is a row or column of a nonogram constrained by the Clues and
% marked with 1 and 0 for full and blank cells respectively.
%
% This uses nonogram_1d_generator to find all possible configurations of the list and the clfpd
% predicate, `tuples_in/2`, to constrain the List to the list of Possibilities.
nonogram_1d(Clues, List) :-
	length(List, L),
	length(Possibility, L),
	findall(Possibility, nonogram_1d_generator(Clues, Possibility), Possibilities),
	tuples_in([List], Possibilities).


% nonogram_1d_generator
% --------------------------------------------------

%% nonogram_1d_generator/2
% nonogram_1d_generator(+Clues, -List)
% True if List is a row or column of a nonogram constrained by the Clues and
% marked with 1 and 0 for full and blank cells respectively.
%
% Equivalent to nonogram_1d_generator/3 where N < 0.
nonogram_1d_generator(Clues, List) :-
	nonogram_1d_generator(Clues, List, -1).

%% nonogram_1d_generator/3
% nonogram_1d_generator(+Clues, -List, +N)
% Same as nonogram_1d_generator/2 with the additional constraint that List begins
% with a range of 1s of length N. If N is 0, List starts with a 0.
% If N < 0, no assertion is made about the first element.

% Base case
% You are allowed to say an empty list starts with a blank cell (N = 0),
% but you cannot say an empty list starts with a full cell (N > 0).
nonogram_1d_generator([], [], N) :- N < 1.

% If N = -1, either the first range starts at the head of List...
nonogram_1d_generator([Clue|Clues], [1|List], -1) :-
	nonogram_1d_generator(Clues, [1|List], Clue).

% ... or the list starts with a blank
nonogram_1d_generator(Clues, [0|List], -1) :-
	nonogram_1d_generator(Clues, List, -1).

% If N = 0, List starts with a blank.
nonogram_1d_generator(Clues, [0|List], 0) :-
	nonogram_1d_generator(Clues, List, -1).

% If N > 0, List starts with a range of 1s.
nonogram_1d_generator(Clues, [1|List], N) :-
	N > 0,
	N_Next is N - 1,
	nonogram_1d_generator(Clues, List, N_Next).


% nonogram_1d_automaton
% --------------------------------------------------

% The original NFA in clfpd implementation was by Dr. Lars Buitinck.

% Copyright (c) 2011 Lars Buitinck.
% Do with this code as you like, but don't remove the copyright notice.

%% nonogram_1d_automaton/2
% nonogram_1d_automaton(?Clues, ?List)
% True if List is a row or column of a nonogram constrained by the Clues and
% marked with 1 and 0 for full and blank cells respectively.
%
% Since the clues describe a finite set of strings of 1s and 0s, we generate an NFA
% to recognize all possible configurations. Clfpd can apply the NFA as a constraint.
nonogram_1d_automaton(Clues, List) :-
	nonogram_1d_automaton_generate(Clues, Arcs, start, FinalState),

	% The automoaton expects that the list to end in 0.
	append(List, [0], ListZ),

	% The automaton expects that the list starts with 1, so we add an arc to allow 0.
	automaton(ListZ, [source(start), sink(FinalState)], [arc(start,0,start) | Arcs]).

%% nonogram_1d_automaton_generate/4
% nonogram_1d_automaton_generate(+Clues, -Arcs, +StartState, -EndState)
% True when the list of Clues describes the same set of strings as an NFA described
% by a list of Arcs. The symbol used for the first state must be given, and the symbol for the
% final state is returned.
%
% The automaton expects that the list starts with 1 and ends with 0.
% TODO: Make this more general / no expectations.
nonogram_1d_automaton_generate([], [], FinalState, FinalState).

nonogram_1d_automaton_generate([0|Clues], Arcs, CurrentState, FinalState) :-
	gensym(state, NextState),
	Arcs = [arc(CurrentState,0,CurrentState), arc(CurrentState,0,NextState) | Rest],
	nonogram_1d_automaton_generate(Clues, Rest, NextState, FinalState).

nonogram_1d_automaton_generate([C|Clues], Arcs, CurrentState, FinalState) :-
	C #> 0,
	C0 #= C - 1,
	gensym(state, NextState),
	Arcs = [arc(CurrentState, 1, NextState) | Rest],
	nonogram_1d_automaton_generate([C0|Clues], Rest, NextState, FinalState).


% Test Code
% ==================================================

nonogram_write(Rows) :- maplist(nonogram_write_writeRow, Rows).
nonogram_write_writeRow([]) :-
	nl.
nonogram_write_writeRow([1|Row]) :-
	write('#'),
	nonogram_write_writeRow(Row).
nonogram_write_writeRow([0|Row]) :-
	write('.'),
	nonogram_write_writeRow(Row).


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
	nonogram_write(Solution),
	!.


%% nonogram_test_big/0
% Solves a 20x20 nonogram
nonogram_test_big :-
	write('Solving a 20x20 nonogram'), nl,
	write('This may take a while'), nl,
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
	nonogram_write(Solution),
	!.
