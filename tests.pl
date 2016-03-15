
:- [stree].


:-
   ((stree([+, x, y], _), !,
   write('Test 1 passed.')) ; write('Test 1 failed.')), nl. 

:-
   ((stree([+, x, [~, x]], _), !,
   write('Test 2 passed.')) ; write('Test 2 failed.')), nl.
    
:-
   ((\+ stree([*, x, [~, x]], _),
   write('Test 3 passed.')) ; write('Test 3 failed.')), nl.
   
       
:-
   ((stree([==>, a, b], _),
   write('Test 4 passed.')) ; write('Test 4 failed.')), nl.
   
% This one is a tautology.   
:-
   ((stree([==>, [*, a, [~, a]],
                 [+, y, z]], _),
   write('Test 5 passed.')) ; write('Test 5 failed.')), nl.
   
% Its negation is unsatisfiable.
:-
   ((\+ stree([~, [==>, [*, a, [~, a]],
                        [+, y, z]]], _),
   write('Test 6 passed.')) ; write('Test 6 failed.')), nl.

