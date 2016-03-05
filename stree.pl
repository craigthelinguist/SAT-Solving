

isvar(X) :-
   \+ is_list(X), \+ isbool(X), \+ isoper(X).

isbool(true).
isbool(false).

negation(true, false).
negation(false, true).

isoper(+).
isoper(*).
isoper(==>).
isoper(~).



%% Set
%% =======================================

emptySet([]).

setAdd(Old, Item, [Item|New]) :-
   setDel(Old, Item, New).

setDel([], _, []).
setDel([Head|Old], Item, [Head|New]) :-
   Item \= Head,
   setDel(Old, Item, New).
setDel([Item|Old], Item, New) :-
   setDel(Old, Item, New).

setUnion([], Set2, Set2).

setUnion([Item|Set1], Set2, Union) :-
   setAdd(Set2, Item, Set3),
   setUnion(Set1, Set3, Union).



%% Map
%% =======================================

mapLookup([Var:Val|_], Var, Val).

mapLookup([Item:_|SubMap], Var, Val) :-
   Item \== Var,
   mapLookup(SubMap, Var, Val).

mapContains([Head:_|SubMap], Var) :-
   Head \== Var,
   mapLookup(SubMap, Var).

mapContains([Var:_|_], Var).

mapExtend(Map, Var, Val, Map3) :-
   setDel(Map, Var:_, Map2),
   setAdd(Map2, Var:Val, Map3).



%% eval(Formula, Map, Result)
%% Holds if Formula evaluates to Result in the context of Map.
%% ==========================================================

% True evaluates to true in any map.
eval(true, _).

% Variables are true if they're true in the map.
eval(X, Map) :-
   mapLookup(Map, X, true).

% OR: true if either argument is true.
eval([+, Arg1, Arg2], Map) :-
   eval(Arg1, Map) ; eval(Arg2, Map).

% AND: true if both arguments are true.
eval([*, Arg1, Arg2], Map) :-
   eval(Arg1, Map), eval(Arg2, Map).

% IMPLIES: true if hypothesis false or conclusion true.
eval([==>, Arg1, Arg2], Map) :-
   eval(Arg2, Map) ; \+ eval(Arg1, Map).



%% freeVars(Formula, FreeVars)
%% Holds if FreeVars is the set of free variables in Formula.
%% ==========================================================

freeVars(X, []) :-
   isbool(X), !.

freeVars(X, [X]) :-
   isvar(X), !.

freeVars([_, Arg], Vars) :-
   freeVars(Arg, Vars), !.

freeVars([_, Arg1, Arg2], Vars) :-
   freeVars(Arg1, Vars1),
   freeVars(Arg2, Vars2),
   setUnion(Vars1, Vars2, Vars), !.


%% stree(Formula, Ans)
%% Holds if Ans is a variable assignment that makes Formula true.
%% ==============================================================

stree(Formula, Ans) :-
   freeVars(Formula, Vars),
   setof(X, stree(Formula, Vars, [], X), Results),
   member(Ans, Results).

stree(Formula, [], Map, Map) :-
   eval(Formula, Map).

stree(Formula, [V|Vars], Map, Ans) :-
   (mapExtend(Map, V, true, Map2), stree(Formula, Vars, Map2, Ans))
   ; (mapExtend(Map, V, false, Map3), stree(Formula, Vars, Map3, Ans)).








