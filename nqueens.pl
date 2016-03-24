


% Holds if D is the number of diagonals on a board of width N.
diagonals(N, D) :-
   D is 2 * (N - 1) + 1.

% Generate the variable name encoding that there is a Queen at
% the given row and column.
var(Row, Col, Var) :-
   atom_number(R, Row),
   atom_number(C, Col),
   atomic_list_concat([b, :, R, :, C], Var).

% Return the Cartesian product of two lists A and B.
cartesian([], _, []).

cartesian([A|As], B, AxB) :-
   cartesian(As, B, SubList),
   findall(Pair, (member(X, B), Pair = A:X), Pairs),
   append(Pairs, SubList, AxB).

% There is a queen on each row.
queenOnRowR(N, Row, Formula) :-
   range(0, N, Cols),
   findall(V, (member(Col, Cols), var(Row, Col, V)), RowVars),
   disjunction(RowVars, Formula).

queenOnAllRows(N, Formula) :-
   range(0, N, Rows),
   findall(F, (member(Row, Rows), queenOnRowR(N, Row, F)), Formulae),
   conjunction(Formulae, Formula).


% Generate a list of numbers from start inclusive to end exclusive.
range(End, End, []) :- !.

range(Start, End, [Start|Rest]) :-
   X is Start + 1,
   range(X, End, Rest), !.

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

