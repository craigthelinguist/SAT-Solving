
:- [sat].


%% Predicates for formatting output.
%% ======================================================================

join([], []).

join([X|Xs], Result) :-
   join(Xs, RHS),
   concat(X, RHS, Result).   

to_ccodes(X, S) :-
   atom(X),
   atom_codes(X, S), !.

to_ccodes([~, X], R) :-
   to_ccodes(X, S1),
   join(["[", "~", ",", " ", S1, "]"], R), !.

to_ccodes([+, X, Y], R) :-
   to_ccodes(X, S1),
   to_ccodes(Y, S2),
   join(["[", "+", ",", " ", S1, ",", " ", S2, "]"], R), !.

to_ccodes([==>, X, Y], R) :-
   to_ccodes(X, S1),
   to_ccodes(Y, S2),
   join(["[", "==>", ",", " ", S1, ",", " ", S2, "]"], R), !.

to_ccodes([*, X, Y], R) :-
   to_ccodes(X, S1),
   to_ccodes(Y, S2),
   join(["[", "*", ",", " ", S1, ",", " ", S2, "]"], R), !.

to_str(F, S) :-
   to_ccodes(F, R),
   atom_codes(S, R).



%% Test and give message based on pass or failure.
%% ======================================================================

test(Predicate, Input, pass) :-

   atom_codes(Predicate, P),
   to_ccodes(Input, I),

    ((call(Predicate, Input, _),
      join(["passed: ", P, "(", I, ")"], CCodes),
      atom_codes(S, CCodes), write(S))
   ; (join(["failed: ", P, "(", I, ")"], CCodes),
      atom_codes(S, CCodes), write(S))),

   nl.

test(Predicate, Input, fail) :-
   
   atom_codes(Predicate, P),
   to_ccodes(Input, I),

    ((\+ call(Predicate, Input, _),
      join(["passed: ", P, "(", I, ")"], CCodes),
      atom_codes(S, CCodes), write(S))
   ; (join(["failed: ", P, "(", I, ")"], CCodes),
      atom_codes(S, CCodes), write(S))),

   nl.

   

%% The tests.
%% ======================================================================

:- test(stree, [+, x, y], pass).

:- test(stree, [+, x, [~, x]], pass).

:- test(stree, [*, x, [~, x]], fail).

:- test(stree, [==>, a, b], pass).

% This one is a tautology.
:- test(stree, [==>, [*, a, [~, a]],
                     [+, y, z]], pass).

% Its negation is unsatisfiable.
:- test(stree, [~, [==>, [*, a, [~, a]],
                         [+, y, z]]], fail).

% Another tautology.
:- test(stree, [+, x,
                   [==>, p,
                         [+, p,
                             [~, x]]]], pass).
              
% Its negation is unsatisfiable.
:- test(stree, [~, [+, x, [==>, p, [+, p, [~, x]]]]], fail).

% Satisfiable by p:false, x:false, y:true
:- test(stree, [*, [*, [~, p], [+, x, y]],
                   [+, [~, p], [==>, p, [+, p, [~, x]]]]], pass).

:- test(stree, [*, [+, [~, p], [+, x, y]], p], pass).

:- test(stree, [==>, p, [+, p, [~, x]]], pass).



