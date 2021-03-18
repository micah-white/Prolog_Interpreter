:- include('fileIO.pl').

splitWords([], []) :- !.
splitWords([X], [X]) :- atom(X), !.
splitWords([H|[X|T]], R) :- not(diffTypes(H, X)), splitWords([X|T], A), append([H], A, R), !.
splitWords([H|[X|T]], R) :- diffTypes(H,X), splitWords([X|T],B), append([H], [' '], A), append(A,B,R).

diffTypes(A,B) :- isOp(A), isOp(B), !, fail.
diffTypes(A,B) :- isAlphaNum(A), isAlphaNum(B), !, fail.
diffTypes(A,B) :- atom(A), atom(B).

combineLetters([], []) :- !.
combineLetters([X], []) :- isWhitespace(X), !.
combineLetters([X], [X]) :- atom(X), !.
combineLetters([H|T], R) :- isWhitespace(H), combineLetters(T, R), !.
combineLetters([H|[X|T]], R) :- atom(H), diffTypes(H,X), combineLetters([X|T],A), append([H], A, R), !.
combineLetters([H|[X|T]], R) :- not(diffTypes(H,X)), combineLetters([X|T], [A|C]), atom_concat(H, A, B), append([B], C, R) .

group1([], []) :- !.
group1(X, X) :- not(member('(', X)), not(member(')', X)), !.
group1(['('|T], [A|Z]) :- groupHelp1(T, (A, B)), group1(B, Z), !.
group1([H|T], [H|Z]) :- group1(T, Z).

groupHelp1([], ([], [])), !.
groupHelp1([')'|T], ([], T)) :- !.
groupHelp1(['('|T], ([A|C], D)) :- groupHelp1(T, (A, B)), groupHelp1(B, (C, D)), !.
groupHelp1([H|T], ([H|A], B)) :- groupHelp1(T, (A, B)).


group2([], []) :- !.
group2(X, X) :- not(member('{', X)), not(member('}', X)), !.
group2(['{'|T], [A|Z]) :- groupHelp2(T, (A, B)), group2(B, Z), !.
group2([H|T], [H|Z]) :- group2(T, Z).

groupHelp2([], ([], [])), !.
groupHelp2(['}'|T], ([], T)) :- !.
groupHelp2(['{'|T], ([A|C], D)) :- groupHelp2(T, (A, B)), groupHelp2(B, (C, D)), !.
groupHelp2([H|T], ([H|A], B)) :- groupHelp2(T, (A, B)).

group(X, R) :- group1(X, A), group2(A, R).
