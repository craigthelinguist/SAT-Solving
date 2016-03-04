
isvar(X) :-
   \+ is_list(X), \+ isbool(X), \+ isoper(X).

isbool(true).
isbool(false).

isoper(+).
isoper(*).
isoper(==>).
isoper(~).

% eval(Formula, Map, Result).
% Checks the result of evaluating the Formula in the context of the given Mapping of variables to booleans.

eval(Var, Map, true) :-
   isvar(Var), lookup(Var, Map, true).

eval([+, Arg1, Arg2], Map, true) :-
   eval(Arg1, Map, true); eval(Arg2, map, true).

eval([*, Arg1, Arg2], Map, true) :-
   eval(Arg1, Map, true), eval(Arg2, map, true).

eval([==>, Arg1, Arg2], Map, true) :-
   \+ eval(Arg1, Map, true); eval(Arg2, map, true).

eval([~, Arg], Map, true) :-
   \+ eval(Arg, Map, true).

eval(true, _, true).

eval(X, Y, false) :- \+ eval(X, Y, true).


% lookup(Var, Map, Ans)
% Returns the value to which Var is mapped (true or false).
% The predicate fails if Var is not bound.
lookup(true, _, true).
lookup(false, _, false).
lookup(Var, [Var:Val|_], Val).
lookup(Var, [_|Rest], Result) :-
   lookup(Var, Rest, Result). 

% isbound(Var, Map)
% Holds if the Map binds the variable with the name Var.
is_bound(Var, [X:_|Rest]) :-
   X \== Var, is_bound(X, Rest).
is_bound(Var, [Var:_|_]).

% zip(Xs, Ys, Pairs).
% Zips two lists Xs and Ys together, pairing their respective elements. The resulting list is Pairs. (Xs and Ys must be the same length)
zip([], [], []) :- !.
zip([X|Xs], [Y|Ys], [X:Y|Acc2]) :-
   zip(Xs, Ys, Acc2).

% contradiction(Map).
% Holds if the Map contains a contradiction in its variables.
contradiction(Map) :-
   contradiction2(Map, []).

contradiction2([], _) :- fail.

contradiction2([Var:Val|_], Seen) :-
   member(Var:_, Seen),
   lookup(Var, Seen, PrevVal),
   PrevVal \== Val.

contradiction2([Var:Val|Map], Seen) :-
   contradiction2(Map, [Var:Val|Seen]).




% reductio(Formula).
% Holds if any variable assignment makes Formula false.
% Not complete.

reductio(Formula) :-
   reductio2(Formula, false, [], _).


reductio2(Var, NewVal, OldMap, NewMap) :-
   NewMap = [Var:NewVal|OldMap],
   contradiction(NewMap).

reductio2([~, Form], false, OldMap, NewMap) :-
   reductio2(Form, true, OldMap, NewMap).
    
reductio2([~, Form], true, OldMap, NewMap) :-
   reductio2(Form, false, OldMap, NewMap).



   
   
   
