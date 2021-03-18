square(Num, Result) :- Result is Num*Num.

map(F, [], []) :- !.
map(F, [H|T], [X|Y]) :- call(F,H,X), map(F,T,Y).

remove(H, [H|T], T) :- !.
remove(X, [H|T], [H|Z]) :- remove(X,T,Z).

not(X) :- \+ X.

removeAll(_, [ ], [ ]).
removeAll(H, [H|T], Z) :- removeAll(H, T, Z).
removeAll(X, [H|T], [H|Z]) :- not(X=H), removeAll(X, T, Z).

permute([ ], [ ]).
permute(L, [H|T]) :- member(H, L), remove(H,L,Z), permute(Z,T).

ascend([ ]).
ascend([_]).
ascend([A,B|T]) :- A=<B, ascend([B|T]).

perm_sort(L, R) :- permute(L,R), ascend(R).

postorder([X], [X]).
postorder([X,L,R],Z) :- postorder(L,A), postorder(R,B), 
						append(B,[X],C), append(A,C,Z).

null([]).

flat([ ], [ ]) :- !.
flat(X, [X]) :- atomic(X).
flat([H|T], R) :- flat(H,X), flat(T, Y), append(X,Y,R).

mem(H, [H|_]) :- !.
mem(X, [_|T]) :- mem(X, T).

head([H|T], H).

tail([H|T], T).

isPunct(A) :- atom(A), mem(A, ['(',')','{','}',',']).

isDigit(A) :- atom(A), mem(A, ['0','1','2','3','4','5','6','7','8','9']).

isNum(A) :- atom(A), atom_chars(A,B), maplist(isDigit, B).

isVar(A) :- atom(A), not(isKeyword(A)), atom_chars(A, B), maplist(isAlpha, B).

isKeyword(A) :- atom(A), mem(A, ['while', 'if','else','print']).

isOperator(A) :- atom(A), mem(A, ['+', '=', '<',  '>', '-', '/', '%', '|', '&',  '*',  '!']).

isOp(A) :- atom(A), atom_chars(A,B), maplist(isOperator, B).

isAlpha(A) :- atom(A), mem(A, ['q', 'w', 'e',  'r',  't',  'y',  'u',  'i',  'o',  'p',  'a',  's',  'd',  'f',  'g',  'h',  'j',  'k',  'l',  'z',  'x',  'c',  'v',  'b',  'n',  'm', 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'Z', 'X', 'C', 'V', 'B', 'N', 'M']).

isAlphaNum(A) :- isAlpha(A).
isAlphaNum(A) :- isNum(A).

isWhitespace(A) :- not(isAlphaNum(A)), not(isPunct(A)), not(isVar(A)), not(isKeyword(A)), not(isOperator(A)).

makeList([X], [X]) :- !.
makeList([H|T], [H|T]) :- !.
makeList(X, [X]).

firstSatisfying([H|T], F, C, H) :- call(F, H, C), !.
firstSatisfying([H|T], F, C, R) :- firstSatisfying(T,F,C,R).

splitBy([], _, ([], [])) :- !.
splitBy([H|T], P, ([], T)) :- H == P, !.
splitBy([H|T], P, ([H|Y], Z)) :- splitBy(T, P, (Y, Z)).

min(A,B,A) :- A =< B, !.
min(A,B,B) :- B < A.

doubleList([], []) :- !.
doubleList(X, [X]).

idekFunction([], []) :- !.
idekFunction([H|T], [[H|T]]) :- not(null(T)), !.
idekFunction(X, X).

dumbestFunctionIveEverWritten([], []) :- !.
dumbestFunctionIveEverWritten([[[H|T]|TS]], [[H|T]|TS]) :- !.
dumbestFunctionIveEverWritten([[H|T]|TS], [[H|T]|TS]) :- !.
dumbestFunctionIveEverWritten([H|T], [[H|T]]).

