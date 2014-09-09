:- use_module(library(clpfd)).


% Helpers
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
% nonogram(+RowClues, +ColClues, -Solution)
% Solves a 2D nonogram given by RowClues and ColClues.
nonogram(RowClues, ColClues, Solution) :-
	nonogram(RowClues, ColClues, Solution, nonogram_1d).

%% nonogram/4
% nonogram(+RowClues, +ColClues, -Solution, +Solver)
% Solves a 2D nonogram given by RowClues and ColClues using the specified Solver, a predicate
% implementing the 1-demensional case. There are multiple methods provided by this library:
% - `nonogram_1d/2` is a clpfd wrapper around `nonogram_1d_naive`. Good performance.
% - `nonogram_1d_naive/2` uses backtracking. Slow!
% - `nonogram_1d_automaton/2` uses clpfd to create an NFA that constrains each row/column. Fastest.
nonogram(RowClues, ColClues, Solution, Solver) :-
	length(RowClues, NRows),
	length(ColClues, NCols),
	matrix(NRows, NCols, Solution),
	transpose(Solution, SolutionT),

	% Apply the 1-demensional case to each row and column.
	maplist(Solver, RowClues, Solution),
	maplist(Solver, ColClues, SolutionT),

	% If the puzzle is ambiguous and the method uses clpfd, we need to pick a solution.
	maplist(label, Solution).


% nonogram_1d
% --------------------------------------------------

%% nonogram_1d/2
% nonogram_1d(+Clues, -List)
% True if List is a row or column of a nonogram constrained by the Clues and
% marked with 1 and 0 for full and blank cells respectively.
%
% This uses `nonogram_1d_naive/2` to find all possible configurations of the list, then uses
% the clpfd predicate, `tuples_in/2`, to constrain the List to the set of Possibilities.
nonogram_1d(Clues, List) :-
	length(List, L),
	length(Possibility, L),
	findall(Possibility, nonogram_1d_naive(Clues, Possibility), Possibilities),
	tuples_in([List], Possibilities).


% nonogram_1d_naive
% --------------------------------------------------

%% nonogram_1d_naive/2
% nonogram_1d_naive(+Clues, -List)
% True if List is a row or column of a nonogram constrained by the Clues and
% marked with 1 and 0 for full and blank cells respectively.
%
% Equivalent to nonogram_1d_naive/3 where N < 0.
nonogram_1d_naive(Clues, List) :-
	nonogram_1d_naive(Clues, List, -1).

%% nonogram_1d_naive/3
% nonogram_1d_naive(+Clues, -List, +N)
% Same as nonogram_1d_naive/2 with the additional constraint that List begins
% with a range of 1s of length N. If N is 0, List starts with a 0.
% If N < 0, no assertion is made about the first element.

% Base case
% You are allowed to say an empty list starts with a blank cell (N = 0),
% but you cannot say an empty list starts with a full cell (N > 0).
nonogram_1d_naive([], [], N) :- N < 1.

% If N = -1, either the first range starts at the head of List...
nonogram_1d_naive([Clue|Clues], [1|List], -1) :-
	nonogram_1d_naive(Clues, [1|List], Clue).

% ... or the list starts with a blank
nonogram_1d_naive(Clues, [0|List], -1) :-
	nonogram_1d_naive(Clues, List, -1).

% If N = 0, List starts with a blank.
nonogram_1d_naive(Clues, [0|List], 0) :-
	nonogram_1d_naive(Clues, List, -1).

% If N > 0, List starts with a range of 1s.
nonogram_1d_naive(Clues, [1|List], N) :-
	N > 0,
	N_Next is N - 1,
	nonogram_1d_naive(Clues, List, N_Next).


% nonogram_1d_automaton
% --------------------------------------------------

% Modified from work by Dr. Lars Buitinck
% https://gist.github.com/larsmans/1146705

% Copyright (c) 2011 Lars Buitinck.
% Do with this code as you like, but don't remove the copyright notice.

%% nonogram_1d_automaton/2
% nonogram_1d_automaton(+Clues, -List)
% True if List is a row or column of a nonogram constrained by the Clues and
% marked with 1 and 0 for full and blank cells respectively.
%
% Since the clues describe a finite set of strings of 1s and 0s, we generate an NFA
% to recognize all possible configurations. Clfpd can apply the NFA as a constraint.
nonogram_1d_automaton(Clues, List) :-
	nonogram_1d_automaton_construct(Clues, Arcs, StartState, FinalState),
	automaton(List, [source(StartState), sink(FinalState)], Arcs).

%% nonogram_1d_automaton_construct/4
% nonogram_1d_automaton_construct(+Clues, -Arcs, -StartState, -EndState)
% True when the list of Clues describes the same set of strings
% as an NFA given as a list of Arcs and both start and end states.
nonogram_1d_automaton_construct(Clues, Arcs, StartState, FinalState):-
	gensym(state, StartState),
	Arcs = [arc(StartState,0,StartState) | Rest],
	nonogram_1d_automaton_construct_(Clues, Rest, StartState, FinalState).

%% nonogram_1d_automaton_construct_/4
% nonogram_1d_automaton_construct_(+Clues, -Arcs, -StartState, -EndState)
% This predicate encapsulates the implementation details of `nonogram_1d_automaton_construct`
nonogram_1d_automaton_construct_([0], [arc(FinalState,0,FinalState)], FinalState, FinalState).

nonogram_1d_automaton_construct_([0|Clues], Arcs, CurrentState, FinalState) :-
	gensym(state, NextState),
	Arcs = [arc(CurrentState,0,CurrentState), arc(CurrentState,0,NextState) | Rest],
	nonogram_1d_automaton_construct_(Clues, Rest, NextState, FinalState).

nonogram_1d_automaton_construct_([C|Clues], Arcs, CurrentState, FinalState) :-
	C #> 0,
	C0 #= C - 1,
	gensym(state, NextState),
	Arcs = [arc(CurrentState, 1, NextState) | Rest],
	nonogram_1d_automaton_construct_([C0|Clues], Rest, NextState, FinalState).








% Test Code
% ==================================================
% Most of the test cases come from [Rosetta Code](http://rosettacode.org/wiki/Nonogram_solver)
% Only use`nonogram_1d_naive` to verify solutions. Solving puzzles is too slow.

%% nonogram_write/1
% nonogram_write(Rows)
% Writes a nonogram.
nonogram_write(Rows) :-
	Rows = [FirstRow|_],
	write('┌'),
	foreach(member(X, FirstRow), write('──')),
	write('┐'),
	nl,
	maplist(nonogram_write_writeRow, Rows),
	write('└'),
	foreach(member(X, FirstRow), write('──')),
	write('┘'),
	nl.

nonogram_write_writeRow(Row) :-
	write('│'),
	nonogram_write_writeRow_(Row),
	write('│'),
	nl.

nonogram_write_writeRow_([]).
nonogram_write_writeRow_([1|Row]) :-
	write('# '),
	nonogram_write_writeRow_(Row).
nonogram_write_writeRow_([0|Row]) :-
	write('  '),
	nonogram_write_writeRow_(Row).


%% nonogram_test_time/2
% Gets the time it takes to run Goal in seconds
nonogram_test_time(Goal, Time) :-
	statistics(cputime, A),
	Goal,
	statistics(cputime, B),
	Time is (B - A).


%% nonogram_test/0
% Run all the tests with all Methods
nonogram_test :-
	forall( member(Solver, [nonogram_1d, nonogram_1d_automaton/*, nonogram_1d_naive*/]), (
		write('Testing '), write(Solver), write('...'), nl,
		tell('/dev/null'),
		time(nonogram_test(Solver)),
		told
	)).

%% nonogram_test/1
% Run all the tests with a given method
nonogram_test(Solver) :-
	nonogram_test_hen(Solver),
	nonogram_test_boat(Solver),
	nonogram_test_beanstalk(Solver),
	nonogram_test_waterBuffalo(Solver),
	nonogram_test_soccer(Solver),
	nonogram_test_ambiguous(Solver),
	!.


%% nonogram_test_hen
% ┌────────────────┐
% │  # # #         │
% │# #   #         │
% │  # # #     # # │
% │    # #     # # │
% │    # # # # # # │
% │#   # # # # #   │
% │# # # # # #     │
% │        #       │
% │      # #       │
% └────────────────┘
nonogram_test_hen :- nonogram_test_hen(nonogram_1d).
nonogram_test_hen(Solver) :-
	nonogram_test_time(
		nonogram(
			[
				[3],
				[2,1],
				[3,2],
				[2,2],
				[6],
				[1,5],
				[6],
				[1],
				[2]
			],
			[
				[1,2],
				[3,1],
				[1,5],
				[7,1],
				[5],
				[3],
				[4],
				[3]
			],
			Solution,
			Solver
		),
		Time
	),
	format('Hen: \n  Solver: ~s\n  Time:   ~3E sec\n', [Solver, Time]),
	nonogram_write(Solution),
	Solution = [
		[0, 1, 1, 1, 0, 0, 0, 0],
		[1, 1, 0, 1, 0, 0, 0, 0],
		[0, 1, 1, 1, 0, 0, 1, 1],
		[0, 0, 1, 1, 0, 0, 1, 1],
		[0, 0, 1, 1, 1, 1, 1, 1],
		[1, 0, 1, 1, 1, 1, 1, 0],
		[1, 1, 1, 1, 1, 1, 0, 0],
		[0, 0, 0, 0, 1, 0, 0, 0],
		[0, 0, 0, 1, 1, 0, 0, 0]
	],
	!.


%% nonogram_test_boat
% ┌────────────────────────────────────────┐
% │                    # # # # # #         │
% │                # # #   #     # # #     │
% │      #     # # #       #         # # # │
% │    # # #   # # # # # # # # # # # # # # │
% │      #     #                         # │
% │    #   #   # #                     # # │
% │# # # # #     # #                 # #   │
% │# # # # #       #                 #     │
% │# # # # #     # # #   # # #   # # #     │
% │# # # # # # # #   # # #   # # #   # # # │
% └────────────────────────────────────────┘
nonogram_test_boat :- nonogram_test_boat(nonogram_1d).
nonogram_test_boat(Solver) :-
	nonogram_test_time(
		nonogram(
			[
				[6],
				[3,1,3],
				[1,3,1,3],
				[3,14],
				[1,1,1],
				[1,1,2,2],
				[5,2,2],
				[5,1,1],
				[5,3,3,3],
				[8,3,3,3]
			],
			[
				[4],
				[4],
				[1,5],
				[3,4],
				[1,5],
				[1],
				[4,1],
				[2,2,2],
				[3,3],
				[1,1,2],
				[2,1,1],
				[1,1,2],
				[4,1],
				[1,1,2],
				[1,1,1],
				[2,1,2],
				[1,1,1],
				[3,4],
				[2,2,1],
				[4,1]
			],
			Solution,
			Solver
		),
		Time
	),
	format('Boat: \n  Solver: ~s\n  Time:   ~3E sec\n', [Solver, Time]),
	nonogram_write(Solution),
	Solution = [
		[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0],
		[0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1],
		[0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
		[0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1],
		[1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0],
		[1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
		[1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0],
		[1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1]
	],
	!.


%% nonogram_test_beanstalk
% ┌────────────────────────────────────────┐
% │        # # #   #                       │
% │        # #   # # # #   #               │
% │        #   # # #   # # #               │
% │    # #   # # # #                       │
% │  # # #   # # #   #         # # #       │
% │# # #     # #   # #       #   # # #     │
% │# #     # #   # #         # #   # #     │
% │        # #   #   #     # #   #   #     │
% │        #   # #   #       # # # #       │
% │        #   #   # #           # #       │
% │          # #   # #     # # # # # # # # │
% │        # #   # #       # #     # # # # │
% │        #   # #   # #   #       #     # │
% │# # #     # # #   # # # # #           # │
% │#   #   # # #   #         #         # # │
% │# #     # # #   #         # # #   # # # │
% │  #   # # #   # #   # # # # # # # #     │
% │  # # # #   # # #   # # # # # # # #     │
% │      #   # # # #   # #   # # # # #     │
% │      #   # # # #   # #       # #       │
% │        # # # #     # #       # # # # # │
% │      # # # # #   # # #       # # # # # │
% │      # # # #   #                     # │
% │    # # # #   # #                       │
% │    # # #   # # #                       │
% └────────────────────────────────────────┘
nonogram_test_beanstalk :- nonogram_test_beanstalk(nonogram_1d).
nonogram_test_beanstalk(Solver) :-
	nonogram_test_time(
		nonogram(
			[
				[3,1],
				[2,4,1],
				[1,3,3],
				[2,4],
				[3,3,1,3],
				[3,2,2,1,3],
				[2,2,2,2,2],
				[2,1,1,2,1,1],
				[1,2,1,4],
				[1,1,2,2],
				[2,2,8],
				[2,2,2,4],
				[1,2,2,1,1,1],
				[3,3,5,1],
				[1,1,3,1,1,2],
				[2,3,1,3,3],
				[1,3,2,8],
				[4,3,8],
				[1,4,2,5],
				[1,4,2,2],
				[4,2,5],
				[5,3,5],
				[4,1,1],
				[4,2],
				[3,3]
			],
			[
				[2,3],
				[3,1,3],
				[3,2,1,2],
				[2,4,4],
				[3,4,2,4,5],
				[2,5,2,4,6],
				[1,4,3,4,6,1],
				[4,3,3,6,2],
				[4,2,3,6,3],
				[1,2,4,2,1],
				[2,2,6],
				[1,1,6],
				[2,1,4,2],
				[4,2,6],
				[1,1,1,1,4],
				[2,4,7],
				[3,5,6],
				[3,2,4,2],
				[2,2,2],
				[6,3]
			],
			Solution,
			Solver
		),
		Time
	),
	format('Jack & the Beanstalk: \n  Solver: ~s\n  Time:   ~3E sec\n', [Solver, Time]),
	nonogram_write(Solution),
	Solution = [
		[0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0],
		[0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
		[0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0],
		[1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0],
		[1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0],
		[0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0],
		[0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0],
		[0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0],
		[0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1],
		[0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1],
		[1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1],
		[1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1],
		[1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1],
		[0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0],
		[0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0],
		[0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0],
		[0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0],
		[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1],
		[0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1],
		[0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
		[0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
		[0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
	],
	!.


%% nonogram_test_waterBuffalo
% ┌──────────────────────────────────────────────────┐
% │                                        # # # # # │
% │    # #                             # # #     # # │
% │  # #                             # # # # #     # │
% │# #                           # # # # # # # #     │
% │# #         # # # # #   # # # # # # # # # # #     │
% │#   #     # #         #         # # # # # #       │
% │#     # #           #               # # #         │
% │# #                 #                           # │
% │  # #           # # # # # #                   # # │
% │    # # # # # # # # # # # # # # #         # # # # │
% │          # # # # # # # # # #     # # # # # # # # │
% │        # #   #   # # # #   # # #     # # # # # # │
% │                # # # # # # # # # # # # # # # # # │
% │                # # # # # # # # # # # # # # # # # │
% │              # # # # # # # # # # # # # # # # # # │
% │              #       # # # # # # # # # # # # # # │
% │              #   #   # # # # # # # # # # # # # # │
% │                # # # # #       # # # # # # # # # │
% │                                  # # # # # # # # │
% │                                    # # # # # # # │
% └──────────────────────────────────────────────────┘
nonogram_test_waterBuffalo :- nonogram_test_waterBuffalo(nonogram_1d).
nonogram_test_waterBuffalo(Solver) :-
	nonogram_test_time(
		nonogram(
			[
				[5],
				[2,3,2],
				[2,5,1],
				[2,8],
				[2,5,11],
				[1,1,2,1,6],
				[1,2,1,3],
				[2,1,1],
				[2,6,2],
				[15,4],
				[10,8],
				[2,1,4,3,6],
				[17],
				[17],
				[18],
				[1,14],
				[1,1,14],
				[5,9],
				[8],
				[7]
			],
			[
				[5],
				[3,2],
				[2,1,2],
				[1,1,1],
				[1,1,1],
				[1,3],
				[2,2],
				[1,3,3],
				[1,3,3,1],
				[1,7,2],
				[1,9,1],
				[1,10],
				[1,10],
				[1,3,5],
				[1,8],
				[2,1,6],
				[3,1,7],
				[4,1,7],
				[6,1,8],
				[6,10],
				[7,10],
				[1,4,11],
				[1,2,11],
				[2,12],
				[3,13]
			],
			Solution,
			Solver
		),
		Time
	),
	format('Water Buffalo: \n  Solver: ~s\n  Time:   ~3E sec\n', [Solver, Time]),
	nonogram_write(Solution),
	Solution = [
		[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1],
		[0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1],
		[0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1],
		[1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0],
		[1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0],
		[1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0],
		[1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0],
		[1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
		[0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1],
		[0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1],
		[0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1]
	],
	!.


%% nonogram_test_soccer
% This is the nonogram example from [Wikipedia](http://en.wikipedia.org/wiki/Nonogram):
% ┌────────────────────────────────────────┐
% │                    # # #               │
% │                  # # # # #             │
% │                  # # #   #             │
% │                  # #     #             │
% │            # # #   # # #   # # # #     │
% │        # #     # #       # # # # # # # │
% │    # # # # # #   #       #             │
% │  # # # #       # #     # #             │
% │                #       #               │
% │              # # #     #               │
% │              # # # # # #               │
% │  # #       # # # # # # #               │
% │# # # # # #     # # #   #               │
% │#   # #     # #   #     #               │
% │      # # # #     #   #     # # #       │
% │                # # # #   # #   # #     │
% │                # # #     # # #   #     │
% │              # # #         # # #       │
% │            # # #                       │
% │            # #   #                     │
% └────────────────────────────────────────┘
nonogram_test_soccer :- nonogram_test_soccer(nonogram_1d).
nonogram_test_soccer(Solver) :-
	nonogram_test_time(
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
			Solution,
			Solver
		),
		Time
	),
	format('Soccer: \n  Solver: ~s\n  Time:   ~3E sec\n', [Solver, Time]),
	nonogram_write(Solution),
	Solution = [
		[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0],
		[0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1],
		[0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
		[0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0],
		[0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0],
		[1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0],
		[1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0],
		[0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0],
		[0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
	],
	!.


%% nonogram_test_ambiguous
% This puzzle is ambiguous. It has 3 solutions:
% ┌────────┐┌────────┐┌────────┐
% │  # #   ││# #     ││# #     │
% │# #     ││# #     ││# #     │
% │#       ││      # ││    #   │
% │      # ││    #   ││      # │
% └────────┘└────────┘└────────┘
nonogram_test_ambiguous :- nonogram_test_ambiguous(nonogram_1d).
nonogram_test_ambiguous(Solver) :-
	nonogram_test_time(
		findall(S, nonogram([[2],[2],[1],[1]], [[2],[2],[1],[1]], S, Solver),  Solutions),
		Time
	),
	format('Ambiguous puzzle: \n  Solver: ~s\n  Time:   ~3E sec\n', [Solver, Time]),
	forall(member(S, Solutions), nonogram_write(S)),
	length(Solutions, 3),
	member([
		[1,1,0,0],
		[1,1,0,0],
		[0,0,1,0],
		[0,0,0,1]
	], Solutions),
	member([
		[1,1,0,0],
		[1,1,0,0],
		[0,0,0,1],
		[0,0,1,0]
	], Solutions),
	member([
		[0,1,1,0],
		[1,1,0,0],
		[1,0,0,0],
		[0,0,0,1]
	], Solutions),
	!.
