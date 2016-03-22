


% Holds if D is the number of diagonals on a board of width N.
diagonals(N, D) :-
   D is 2 * (N - 1) + 1.

% Generate the variable name encoding that there is a Queen at
% the given row and column.
var(Row, Col, Var) :-
   atom_number(R, Row),
   atom_number(C, Col),
   atomic_list_concat([b, :, R, :, C], Var).
   
