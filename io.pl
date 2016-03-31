
% Save the Prolog Term to the file Fname.
save(Fname, Term) :-
   open(Fname, write, Stream),
   write(Stream, Term), put_char(Stream, '.'),
   close(Stream).

% Load the Prolog Term from the file Fname.
load(Fname, Term) :-
   open(Fname, read, Stream),
   read(Stream, Term), close(Stream).

