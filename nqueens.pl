


nqueens(N, F) :-
   queenOnAllRows(N, Q1),
   oneQueenOnAllRows(N, Q2),
   queenOnAllCols(N, Q3),
   oneQueenOnAllCols(N, Q4),
   oneQueenOnAllLeftDiagonals(N, Q5),
   oneQueenOnAllRightDiagonals(N, Q6),
   L = [Q1, Q2, Q3, Q4, Q5, Q6],
   conjunction(L, F), !.


% Generate the variable name encoding that there is a Queen at
% the given row and column.
var(Row, Col, Var) :-
   atom_number(R, Row),
   atom_number(C, Col),
   atomic_list_concat([b, ., R, ., C], Var).



%% Exactly one queen on each row.
%% ============================================================

queenOnRowR(N, Row, Formula) :-
   range(0, N, Cols),
   findall(V, (member(Col, Cols), var(Row, Col, V)), RowVars),
   disjunction(RowVars, Formula).

queenOnAllRows(N, Formula) :-
   range(0, N, Rows),
   findall(F, (member(Row, Rows), queenOnRowR(N, Row, F)), Result),
   flatten(Result, Formulae), % flatten if only one result
   conjunction(Formulae, Formula).

oneQueenOnRowR(N, Row, Formula) :-
   range(0, N, Cols),
   findall(V, (member(Col, Cols), var(Row, Col, V)), RowVars),
   distinctPairs(RowVars, RowVars, Pairs),
   findall(X, (member(P, Pairs), notBoth(P, X)), Formulae),
   conjunction(Formulae, Formula). 

oneQueenOnAllRows(N, Formula) :-
   range(0, N, Rows),
   findall(F, (member(Row, Rows), oneQueenOnRowR(N, Row, F)), Formulae),
   conjunction(Formulae, Formula).
   


%% Exactly one queen on each column.
%% ============================================================

queenOnColC(N, Col, Formula) :-
   range(0, N, Rows),
   findall(V, (member(Row, Rows), var(Row, Col, V)), ColVars),
   disjunction(ColVars, Formula).

queenOnAllCols(N, Formula) :-
   range(0, N, Cols),
   findall(F, (member(Col, Cols), queenOnColC(N, Col, F)), Result),
   flatten(Result, Formulae), % flatten if only one result
   conjunction(Formulae, Formula).

oneQueenOnColC(N, Col, Formula) :-
   range(0, N, Rows),
   findall(V, (member(Row, Rows), var(Row, Col, V)), ColVars),
   distinctPairs(ColVars, ColVars, Pairs),
   findall(X, (member(P, Pairs), notBoth(P, X)), Formulae),
   conjunction(Formulae, Formula).

oneQueenOnAllCols(N, Formula) :-
   range(0, N, Cols),
   findall(F, (member(Col, Cols), oneQueenOnColC(N, Col, F)), Formulae),
   conjunction(Formulae, Formula).



%% Exactly one queen one each left diagonal.
%% ============================================================

% Enumerate distinct pairs A:B on the I'th diagonal.
% Return the formula [~, [*, A, B]].
oneQueenOnLeftDiagonal(N, I, Formula) :-
   leftDiagonals(N, I, DiagVars),
   distinctPairs(DiagVars, DiagVars, Pairs),
   findall(X, (member(P, Pairs), notBoth(P, X)), Formulae),
   conjunction(Formulae, Formula).

% The conjunction of oneQueenOnLeftDiagonal, for each diagonal.
oneQueenOnAllLeftDiagonals(N, Formula) :-
   numDiagonals(N, NumDiags),
   range(0, NumDiags, Diagonals),
   findall(F, (member(Diag, Diagonals), oneQueenOnLeftDiagonal(N, Diag, F)), Formulae),
   conjunction(Formulae, Formula).

oneQueenOnRightDiagonal(N, I, Formula) :-
   rightDiagonals(N, I, DiagVars),
   distinctPairs(DiagVars, DiagVars, Pairs),
   findall(X, (member(P, Pairs), notBoth(P, X)), Formulae),
   conjunction(Formulae, Formula).

oneQueenOnAllRightDiagonals(N, Formula) :-
   numDiagonals(N, NumDiags),
   range(0, NumDiags, Diagonals),
   findall(F, (member(Diag, Diagonals), oneQueenOnRightDiagonal(N, Diag, F)), Formulae),
   conjunction(Formulae, Formula).



% A board of size 1 has 1 diagonal. Each extra unit of size adds 2 diagonals.
numDiagonals(N, NumDiags) :-
   NumDiags is 2 * (N - 1) + 1.



%% Enumerating diagonals.
%% ============================================================


% ----- diagonals
% Returns the list of variables on the I'th diagonal on an NxN board.
% Diagonal 0: {(0, N-1)} (the top-right).
% Diagonal 1: {(0, N-2), (1, N-1)}
% Diagonal 2: {(0, N-3), (1, N-2), (2, N-1)}, and so on.

leftDiagonals(N, I, Diags) :-

   % Compute number of tiles on the I'th diagonal.
   % The middle diagonal has N tiles.
   % So a tile that is distance D from the middle tile has N - D tiles.
   numDiagonals(N, NumDiags),
   MidDiag is (NumDiags - 1)/2,
   Distance is abs(MidDiag - I),
   NumTiles is N - Distance,

   % Enumerate tiles on this diagonal.
   StartRow is max(N-1-I, 0),
   StartCol is max(0, I-(N-1)),
   enumDiagonals(NumTiles, StartRow:StartCol, 1:1, Diags).

rightDiagonals(N, I, Diags) :-
   numDiagonals(N, NumDiags),
   MidDiag is (NumDiags - 1)/2,
   Distance is abs(MidDiag - I),
   NumTiles is N - Distance,

   StartRow is max(0, I-MidDiag),
   StartCol is min(N-1, I),
   enumDiagonals(NumTiles, StartRow:StartCol, 1:(-1), Diags).

max(A, B, A) :- A > B, !.
max(A, B, A) :- A is B, !.
max(A, B, B) :- B > A, !.
min(A, B, A) :- A < B, !.
min(A, B, A) :- A is B, !.
min(A, B, B) :- B < A, !.

% ----- enumDiagonals
% These predicates go in a diagonal line, from top-left to bottom-right,
% enumerating all the variables on the board. Counter is the number of
% variables to enumerate. StartRow:StartCol is where you should begin.
enumDiagonals(Counter, StartRow:StartCol, RowInc:ColInc, Diags) :-
   enumDiagonals(Counter, StartRow:StartCol, RowInc:ColInc, [], Diags).

enumDiagonals(0, _, _, Diags, Diags) :- !.

enumDiagonals(Counter, Row:Col, RowInc:ColInc, Acc, Diags) :-
   var(Row, Col, V),
   % next diagonal is the tile to bottom-right of this one.
   Row2 is Row + RowInc, Col2 is Col + ColInc, Counter2 is Counter-1,
   enumDiagonals(Counter2, Row2:Col2, RowInc:ColInc, [V|Acc], Diags).












% Generate a list of numbers from start inclusive to end exclusive.
range(End, End, []) :- !.

range(Start, End, [Start|Rest]) :-
   X is Start + 1,
   range(X, End, Rest), !.

% Return the disjunction over the given formulae.


% Return the conjunction over the given formulae.
disjunction(X, Result) :- atom(X), disjunction2(X, Result).
disjunction([], []).
disjunction(Formulae, Result) :-
   clean(Formulae, Clean),
   disjunction2(Clean, Result).

% empty list is empty list.
disjunction([], []).

% atoms and formulae are themselves.
disjunction2(X, X) :- atom(X), !.
disjunction2([*, A, B], [*, A, B]) :- !.
disjunction2([+, A, B], [+, A, B]) :- !.
disjunction2([==>, A, B], [==>, A, B]) :- !.
disjunction2([~, A], [~, A]) :- !.

% singleton is itself.
disjunction2([L], L) :- !.

% if you're down to the last two lists.
disjunction2([F, []], F) :- !.
disjunction2([[], G], G) :- !.

% ignore empty lists.
disjunction2([[]|Rest], Conj) :-
   disjunction2(Rest, Conj), !.

% flatten atoms.
disjunction2([[X]|Rest], Conj) :-
   atom(X),
   disjunction2([X|Rest], Conj), !.

%  OR atom onto conjunction of other lists.
disjunction2([X|Rest], Conj) :-
   atom(X),
   disjunction2(Rest, Conj2),
   Conj = [+, X, Conj2], !.

% OR singleton onto conjunction of the other lists.
disjunction2([[X]|Rest], Conj) :-
   disjunction2(Rest, Conj2),
   Conj = [+, X, Conj2], !.

disjunction2([F|Rest], Conj) :-
   disjunction2(Rest, Conj2),
   Conj = [+, F, Conj2], !.




% Return the conjunction over the given formulae.
conjunction(X, Result) :- atom(X), conjunction2(X, Result).
conjunction([], []).
conjunction(Formulae, Result) :-
   clean(Formulae, Clean),
   conjunction2(Clean, Result).


% empty list is empty list.
conjunction2([], []).

% atoms and formulae are themselves.
conjunction2(X, X) :- atom(X), !.
conjunction2([*, A, B], [*, A, B]) :- !.
conjunction2([+, A, B], [+, A, B]) :- !.
conjunction2([==>, A, B], [==>, A, B]) :- !.
conjunction2([~, A], [~, A]) :- !.

% singleton is itself.
conjunction2([L], L) :- !.

% if you're down to the last two lists.
conjunction2([F, []], F) :- !.
conjunction2([[], G], G) :- !.

% ignore empty lists.
conjunction2([[]|Rest], Conj) :-
   conjunction2(Rest, Conj), !.

% flatten atoms.
conjunction2([[X]|Rest], Conj) :-
   atom(X),
   conjunction2([X|Rest], Conj), !.

%  AND atom onto conjunction of other lists.
conjunction2([X|Rest], Conj) :-
   atom(X),
   conjunction(Rest, Conj2),
   Conj = [*, X, Conj2], !.

% AND singleton onto conjunction of the other lists.
conjunction2([[X]|Rest], Conj) :-
   conjunction(Rest, Conj2),
   Conj = [*, X, Conj2], !.

conjunction2([F|Rest], Conj) :-
   conjunction(Rest, Conj2),
   Conj = [*, F, Conj2], !.



%conjunction([[X]|Rest], Conj) :-
%   atom(X), conjunction([X|Rest], Conj), !.

%conjunction([[X]|Rest], Conj) :-
%   conjunction([X|Rest], Conj), !.

%conjunction([F], [F]) :- !.

%conjunction([[F], G], Conj) :-
%   atom(F), conjunction([F, G], Conj), !.

%conjunction([F, [G]], Conj) :-
%   atom(G), conjunction([F, G], Conj), !.

%conjunction([F, []], F) :- !.

%conjunction([[], G], G) :- !.

%conjunction([F, G], [*, F, G]) :- !.

%conjunction([F|Rest], [*, F, Conj]) :-
%   conjunction(Rest, Conj), !.

notBoth(A:B, [~, [*, A, B]]).





% Remove empty lists from a list of formulae.
clean([], []) :- !.
clean([[]|List], Clean) :- clean(List, Clean), !.
clean([X|List], [X|Clean]) :- clean(List, Clean), !.


% Return the Cartesian product of two lists A and B.
cartesian([], _, []).

cartesian([A|As], B, AxB) :-
   cartesian(As, B, SubList),
   findall(Pair, (member(X, B), Pair = A:X), Pairs),
   append(Pairs, SubList, AxB).

% Discard pairs in a list where the two components are the same.
theSame(A:B, B:A).

discardDupes(List, Result) :-
   discardDupes(List, [], Result).

discardDupes([], _, []).
   
discardDupes([A:A|List], Acc, Rest) :-  
   discardDupes(List, Acc, Rest), !.

discardDupes([X|List], Acc, Rest) :-
   member(Y, Acc),
   theSame(X, Y),
   discardDupes(List, Acc, Rest), !.

discardDupes([X|List], Acc, [X|Rest]) :-
   discardDupes(List, [X|Acc], Rest), !.

% Find Cartesian product where the two components are distinct.
distinctPairs(A, B, Pairs) :-
   cartesian(A, B, AxB),
   discardDupes(AxB, Pairs), !.

flatten([], []).

flatten([[X]|List], [X|List2]) :-
   atom(X),
   flatten(List, List2), !.

flatten([X|List], [X|List2]) :-
   flatten(List, List2), !.


