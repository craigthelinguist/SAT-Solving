
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

   

%% Satisfiability tests using semantic tree method.
%% ======================================================================

% Simple connectives.
:- test(satisfiable, x, pass).
:- test(satisfiable, [*, x, y], pass).
:- test(satisfiable, [~, x], pass).
:- test(satisfiable, [~, [+, x, y]], pass).
:- test(satisfiable, [~, [*, x, y]], pass).
:- test(satisfiable, [~, [==>, x, y]], pass).
:- test(satisfiable, [+, x, y], pass).
:- test(satisfiable, [+, x, [~, x]], pass).
:- test(satisfiable, [*, x, [~, x]], fail).
:- test(satisfiable, [==>, a, b], pass).

% Tautologies.
:- test(satisfiable, [==>, [*, a, [~, a]], [+, y, z]], pass).
:- test(satisfiable, [+, x, [==>, p, [+, p, [~, x]]]], pass).
:- test(satisfiable, [==>, p, [+, p, [~, x]]], pass).

% Negation of above tautologies.
:- test(satisfiable, [~, [==>, [*, a, [~, a]], [+, y, z]]], fail).
:- test(satisfiable, [~, [+, x, [==>, p, [+, p, [~, x]]]]], fail).

% Contingent formulae.
% Satisfiable by e.g. p:false, x:false, y:true
:- test(satisfiable, [*, [*, [~, p], [+, x, y]],
                   [+, [~, p], [==>, p, [+, p, [~, x]]]]], pass).
:- test(satisfiable, [*, [+, [~, p], [+, x, y]], p], pass).



%% Falsifiability tests using semantic tableau method.
%% ======================================================================

% Simple connectives.
:- test(falsifiable, x, pass).
:- test(falsifiable, [*, x, y], pass).
:- test(falsifiable, [+, x, y], pass).
:- test(falsifiable, [==>, x, y], pass).
:- test(falsifiable, [~, x], pass).
:- test(falsifiable, [~, [+, x, y]], pass).
:- test(falsifiable, [~, [*, x, y]], pass).
:- test(falsifiable, [~, [==>, x, y]], pass).

% Tautologies are not falsifiable.
:- test(falsifiable, [==>, [*, a, [~, a]],
                     [+, y, z]], fail).
:- test(falsifiable, [+, x, [==>, p, [+, p, [~, x]]]], fail).
:- test(falsifiable, [+, x, [~, x]], fail).
:- test(falsifiable, [==>, p, [+, p, [~, x]]], fail).

% Negation of above tautologies.
:- test(falsifiable, [~, [==>, [*, a, [~, a]], [+, y, z]]], pass).
:- test(falsifiable, [~, [+, x, [==>, p, [+, p, [~, x]]]]], pass).

% Contingent formulae.
:- test(falsifiable, [*, [*, [~, p], [+, x, y]],
                   [+, [~, p], [==>, p, [+, p, [~, x]]]]], pass).
:- test(falsifiable, [*, [+, [~, p], [+, x, y]], p], pass).

% Contradictions.
:- test(falsifiable, [*, x, [~, x]], pass).
:- test(falsifiable, [==>, x, [~, x]], pass).
