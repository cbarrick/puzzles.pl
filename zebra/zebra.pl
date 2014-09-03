%% The Zebra Problem
%
% > 1. There are five houses.
% > 2. The Englishman lives in the red house.
% > 3. The Spaniard owns the dog.
% > 4. Coffee is drunk in the green house.
% > 5. The Ukrainian drinks tea.
% > 6. The green house is immediately to the right of the ivory house.
% > 7. The Old Gold smoker owns snails.
% > 8. Kools are smoked in the yellow house.
% > 9. Milk is drunk in the middle house.
% > 10. The Norwegian lives in the first house.
% > 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
% > 12. Kools are smoked in the house next to the house where the horse is kept.
% > 13. The Lucky Strike smoker drinks orange juice.
% > 14. The Japanese smokes Parliaments.
% > 15. The Norwegian lives next to the blue house.
% >
% > Now, who drinks water? Who owns the zebra?
% >
% > In the interest of clarity, it must be added that each of the five houses is painted a
% > different color, and their inhabitants are of different national extractions, own different
% > pets, drink different beverages and smoke different brands of American cigarets [sic]. One
% > other thing: in statement 6, right means your right.
%
% — Life International, December 17, 1962

%% zebra/1
% zebra(HOUSES)
% True if HOUSES is a list of houses satisfying the zebra problem.
% A house is a list: [Color, Nationality, Cigarete, Drink, Pet].
zebra(HOUSES) :-
	length(HOUSES, 5),                                            % 1
	member([red, english, _, _, _], HOUSES),                      % 2
	member([_, spanish, _, _, dog], HOUSES),                      % 3
	member([green, _, _, coffee, _], HOUSES),                     % 4
	member([_, ukrainian, _, tea, _], HOUSES),                    % 5
	left_of([ivory|_], [green|_], HOUSES),                        % 6
	member([_, _, old-gold, _, snails], HOUSES),                  % 7
	member([yellow, _, kool, _, _], HOUSES),                      % 8
	nth1(3, HOUSES, [_, _, _, milk, _]),                          % 9
	nth1(1, HOUSES, [_, norwegian, _, _, _]),                     % 10
	near([_, _, chesterfield, _, _], [_, _, _, _, fox], HOUSES),  % 11
	near([_, _, kool, _, _], [_, _, _, _, horse], HOUSES),        % 12
	member([_, _, lucky-stripe, orange-juice, _], HOUSES),        % 13
	member([_, japanese, parliaments, _, _], HOUSES),             % 14
	near([_, norwegian, _, _, _], [blue, _, _, _, _], HOUSES),    % 15

	% Someone must drink water, and someone must own a zebra.
	member([_, _, _, water, _], HOUSES),
	member([_, _, _, _, zebra], HOUSES),

	% There is only one solution.
	!
	.


%% left_of(A, B, List)
left_of(A, B, [A, B|_]).
left_of(A, B, [_|X]) :- left_of(A, B, X).

%% near(A, B, List)
% True if A and B are beside each other in List.
near(A, B, List) :- left_of(A, B, List) ; left_of(B, A, List).
