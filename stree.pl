

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


isFormula(F) :- atom(F), \+ is_list(F), !.
isFormula([~, _]) :- !.
isFormula([Conn, _, _]) :- isoper(Conn), !.

negative_literal([~, X]) :- atom(X).
positive_literal(X) :- \+ is_list(X), atom(X).
not_literal(F) :- \+ negative_literal(F), \+ positive_literal(F).

emptylist([]).


%% concat(L1, L2, Result)
%% Holds if Result is the concatenation of list1 and list2.
%% ==========================================================

concat([], L2, L2).

concat([X|L1], L2, [X|L3]) :-
   concat(L1, L2, L3).



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



%% Predicates for describing Paths in a semantic tableau.
emptyPath([]:[]).

makePath(Literals, Formulae, Literals:Formulae).
pathLiterals(Literals:_, Literals).
pathFormulae(_:Formulae, Formulae).

% Add a single formula to the path.
pathExtend(Ls:Fs, F, Ls:[F|Fs]) :-
   isFormula(F), !.

% Add a list of formulae to the path.
pathExtend(Ls:Fs, NewFs, Ls:Fs2) :-
   \+ isFormula(NewFs),
   concat(NewFs, Fs, Fs2), !.

pathAddLiteral(Ls:Fs, L, Ls2:Fs) :-
   positive_literal(L),
   Ls2 = [L:true|Ls], !.

pathAddLiteral(Ls:Fs, [~, L], Ls2:Fs) :-
   positive_literal(L),
   Ls2 = [L:false|Ls], !.
   
pathIsClosed([]:_) :- !.
   
pathIsClosed(Path) :-
   pathLiterals(Path, Literals),
   member(Var:Val, Literals),
   negation(Val, NegVal),
   member(Var:NegVal, Literals), !.

pathHasNoFormulae(_:Fs) :-
   emptylist(Fs).

updatePath(Ls:Fs, NewFs, NewLs, Ls2:Fs2) :-
   concat(NewFs, Fs, Fs2),
   concat(NewLs, Ls, Ls2).

closedPaths(Paths) :-
   member(Path, Paths),
   pathIsClosed(Path).

   

%% applyRule(Formula, NewFormulae, NewLiterals, NewPaths)   
%% Rules for how to expand formulae. The various parameters are
%% those extra things added to the semantic tableau as a result
%% of expansion.


 %%  applyRule(Formula, Path, NewPaths),

% Conjunction.
applyRule([*, P, Q], Ls:Fs, [Ls:Fs2]) :-
   Fs2 = [[P, Q]|Fs].

% Disjunction.
applyRule([+, P, Q], Ls:Fs, [Path1, Path2]) :-
   makePath(Ls, [P|Fs], Path1),
   makePath(Ls, [Q|Fs], Path2).

% Implication.
applyRule([==>, P, Q], Ls:Fs, [Ls:Fs2]) :-
   Fs2 = [[+, [~, P], Q]|Fs].
   
% Negative literal.
applyRule([~, X], Ls:Fs, [Ls2:Fs]) :-
   positive_literal(X),
   Ls2 = [X:false|Ls].
   
% Positive literal.
applyRule(X, Ls:Fs, [Ls2:Fs]) :-
   positive_literal(X),
   Ls2 = [X:true|Ls].

% Negation over conjunction.
applyRule([~, [*, P, Q]], Ls:Fs, [Ls:Fs2]) :-
   Fs2 = [[+, [~, P], [~, Q]]|Fs].

% Negation over disjunction.
applyRule([~, [+, P, Q]], Ls:Fs, [Ls:Fs2]) :-
   Fs2 = [[*, [~, P], [~, Q]]|Fs].



%% selectPath(Tableau, Path, NewTableau)
%% Extract a (non-empty) Path from the Tableau. NewTableau is the
%% same, but with the extracted Path and any empty Paths removed.

%% Select first non-empty path.
selectPath([P|TheRest], P, Answer) :-
   \+ pathHasNoFormulae(P),
   delEmptyPaths(TheRest, Answer), !.

%% Extracts first non-empty path from the Tableau.
selectPath([P|Rest], Result, NewTableau) :-
   pathHasNoFormulae(P),
   selectPath(Rest, Result, NewTableau).

% Select first formula.
selectFormula(Ls:[F|Fs], F, Ls:Fs).

% Remove any empty paths from the Tableau.
delEmptyPaths([], []) :- !.

delEmptyPaths([P|Tableau], Rest) :-
   pathHasNoFormulae(P),
   delEmptyPaths(Tableau, Rest).

delEmptyPaths([P|Tableau], [P|Rest]) :-
   delEmptyPaths(Tableau, Rest), !.

%% tableau(Formula)
%% SAT solve a Formula by performing semantic tableau on its negation.

%% Set up the semantic tableau and solve.
tableau(Formula) :-
   emptyPath(P),
   pathExtend(P, [~, Formula], P2),
   \+ solve([P2]). % Satisfiable if negation is not satisfiable.

%% When you run out of paths to expand.
solve([]) :- fail.

%% Solve a Tableau with paths to expand.
solve(Tableau) :-
   selectPath(Tableau, Path, Tableau2),
   selectFormula(Path, Formula, Path2),
   applyRule(Formula, Path2, NewPaths),
   closedPaths(NewPaths),
   concat(NewPaths, Tableau2, Tableau3),
   solve(Tableau3).

