:- include('functions.pl').

% copyfile(Infile, Outfile) :- see(Infile), tell(Outfile), copy, seen, told.

% copy(S) :- get_char(C), loop(C, S).

% loop(end_of_file, S) :- !.
% loop(C, S) :- singleton(C), put_char(C),  copy(R).

getFile(R) :- get_char(C), C \== end_of_file, getFile(A), atom_concat(C, A, R), !.
getFile(R) :- get_char(C), C == end_of_file, R = ' '.

fileToString(InFile, OutString) :- see(InFile), getFile(T), seen, atom_chars(T,OutString).
