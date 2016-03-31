




nqueens(1, V) :-
   var(0, 0, V), !.

nqueens(N, F) :-
   queenOnAllRows(N, Q1),
   oneQueenOnAllRows(N, Q2),
   queenOnAllCols(N, Q3),
   oneQueenOnAllCols(N, Q4),
   L = [Q1, Q2, Q3, Q4],
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
   findall(F, (member(Row, Rows), queenOnRowR(N, Row, F)), Formulae),
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
   findall(F, (member(Col, Cols), queenOnColC(N, Col, F)), Formulae),
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


%% Exactly one queen on each diagonal.
%% ============================================================

% Enumerate distinct pairs A:B on the I'th diagonal.
% Return the formula [~, [*, A, B]].
oneQueenOnDiagonal(N, I, Formula) :-
   diagonals(N, I, DiagVars),
   distinctPairs(DiagVars, DiagVars, Pairs),
   findall(X, (member(P, Pairs), notBoth(P, X)), Formulae),
   conjunction(Formulae, Formula).

% The conjunction of oneQueenOnDiagonal, for each diagonal.
oneQueenOnAllDiagonals(N, Formula) :-
   numDiagonals(N, NumDiags),
   range(0, NumDiags, Diagonals),
   findall(F, (member(Diag, Diagonals), oneQueenOnDiagonal(N, Diag, F)), Formulae),
   conjunction(Formulae, Formula).

% A board of size 1 has 1 diagonal. Each extra unit of size adds 2 diagonals.
numDiagonals(N, NumDiags) :-
   NumDiags is 2 * (N - 1) + 1.



%% Enumerating diagonals.
%% ============================================================
% Diagonal 0: {(0, N-1)} (the top-right).
% Diagonal 1: {(0, N-2), (1, N-1)}
% Diagonal 2: {(0, N-3), (1, N-2), (2, N-1)}, and so on.

% ----- diagonals
% Returns the list of variables on the I'th diagonal on an NxN board.
diagonals(N, I, Diags) :-

   % Compute number of tiles on the I'th diagonal.
   % The middle diagonal has N tiles.
   % So a tile that is distance D from the middle tile has N - D tiles.
   numDiagonals(N, NumDiags),
   MidDiag is (NumDiags - 1)/2,
   Distance is abs(MidDiag - I),
   NumTiles is N - Distance,

   % Enumerate tiles on this diagonal.
   StartRow is 0, StartCol is abs(N-1-I),
   enumDiagonals(NumTiles, StartRow:StartCol, Diags).


% ----- enumDiagonals
% These predicates go in a diagonal line, from top-left to bottom-right,
% enumerating all the variables on the board. Counter is the number of
% variables to enumerate. StartRow:StartCol is where you should begin.
enumDiagonals(Counter, StartRow:StartCol, Diags) :-
   enumDiagonals(Counter, StartRow:StartCol, [], Diags).

enumDiagonals(0, _, Diags, Diags) :- !.

enumDiagonals(Counter, Row:Col, Acc, Diags) :-
   var(Row, Col, V),
   % next diagonal is the tile to bottom-right of this one.
   Row2 is Row+1, Col2 is Col+1, Counter2 is Counter-1,
   enumDiagonals(Counter2, Row2:Col2, [V|Acc], Diags).












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

conjunction([], []).

conjunction(x, x) :- !.
conjunction([F], [F]) :- !.

conjunction([[]|Rest], Conj) :-
   conjunction(Rest, Conj), !.

conjunction([F, []], F) :- !.

conjunction([[], G], G) :- !.

conjunction([F, G], [*, F, G]) :- !.

conjunction([F|Rest], [*, F, Conj]) :-
   conjunction(Rest, Conj), !.

notBoth(A:B, [~, [*, A, B]]).




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





