


% Holds if D is the number of diagonals on a board of width N.
diagonals(N, D) :-
   D is 2 * (N - 1) + 1.

% Generate the variable name encoding that there is a Queen at
% the given row and column.
var(Row, Col, Var) :-
   atom_number(R, Row),
   atom_number(C, Col),
   atomic_list_concat([b, :, R, :, C], Var).

% Return the disjunction over the given formulae.
disjunction([F], [F]) :- !.
disjunction(x, x) :- !.

disjunction([F, G], [+, F, G]) :- !.

disjunction([F|Rest], [+, F, Disj]) :-
   disjunction(Rest, Disj), !.

% Return the conjunction over the given formulae.
conjunction([F], [F]) :- !.
conjunction(x, x) :- !.

conjunction([F, G], [*, F, G]) :- !.

conjunction([F|Rest], [*, F, Conj]) :-
   conjunction(Rest, Conj), !.

