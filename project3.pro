interpret(S) :- fileToString(S,A), combineLetters(A,B), group(B, C), createStatements(C, D), display('\n'), executeStatements(D).

square(Num, Result) :- Result is Num*Num.

map(_, [], []) :- !.
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

head([H|_], H).

tail([_|T], T).

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

firstSatisfying([H|_], F, C, H) :- call(F, H, C), !.
firstSatisfying([_|T], F, C, R) :- firstSatisfying(T,F,C,R).

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



getFile(R) :- get_char(C), C \== end_of_file, getFile(A), atom_concat(C, A, R), !.
getFile(R) :- get_char(C), C == end_of_file, R = ' '.

fileToString(InFile, OutString) :- see(InFile), getFile(T), seen, atom_chars(T,OutString).

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

groupHelp1([], ([], [])) :- !.
groupHelp1([')'|T], ([], T)) :- !.
groupHelp1(['('|T], ([A|C], D)) :- groupHelp1(T, (A, B)), groupHelp1(B, (C, D)), !.
groupHelp1([H|T], ([H|A], B)) :- groupHelp1(T, (A, B)).


group2([], []) :- !.
group2(X, X) :- not(member('{', X)), not(member('}', X)), !.
group2(['{'|T], [A|Z]) :- groupHelp2(T, (A, B)), group2(B, Z), !.
group2([H|T], [H|Z]) :- group2(T, Z).

groupHelp2([], ([], [])) :- !.
groupHelp2(['}'|T], ([], T)) :- !.
groupHelp2(['{'|T], ([A|C], D)) :- groupHelp2(T, (A, B)), groupHelp2(B, (C, D)), !.
groupHelp2([H|T], ([H|A], B)) :- groupHelp2(T, (A, B)).

group(X, R) :- group1(X, A), group2(A, R).


createStatements([], []) :- !.
createStatements(['print'|T], [['print'|A]|C]) :- createPrintStatement(T, (A, B)), createStatements(B, C),!.
createStatements(['if'|T], [['if'|S]|B]) :- createIfStatement(T, (S, A)), createStatements(A, B), !.
createStatements(['while'|T], [['while'|S]|B]) :- createWhileStatement(T, (S, A)), createStatements(A, B), !.
createStatements([H|[T|TS]], [['assign'|[H|[Y]]]|B]) :- isVar(H), ==(T, '='), mathExpression(TS, (Y, A)), createStatements(A, B), !.
createStatements([[H|T]|TS], [Y|TS]) :- createStatements([H|T], Y).

createPrintStatement(L, ([A|C], Z)) :- mathExpression(L, (A, [','|B])), createPrintStatement(B, (C, Z)), !.
createPrintStatement(L, (Y, Z)) :- mathExpression(L, (A, Z)), idekFunction(A, B), makeList(B,Y). 

createIfStatement(L, ([V|[X|[Y]]], Z)) :- mathExpression(L, (V, A)), getStatement(A, (X, ['else'|B])), getStatement(B, (Y, Z)), !.
createIfStatement([H|T], ([X|[Y]], Z)) :- mathExpression(H, (X, _)), getStatement(T, (Y, Z)).

createWhileStatement([H|T], ([X|[Y]], Z)) :- mathExpression(H, (X, _)), getStatement(T, (Y, Z)).

getStatement([], ([], [])) :- !.
getStatement(['print'|T], (['print'|A], B)) :- createPrintStatement(T, (A, B)), !.
getStatement(['if'|T], (['if'|S], A)) :- createIfStatement(T, (S, A)), !.
getStatement(['while'|T], R) :- createWhileStatement(T, R), !.
getStatement([H|[T|TS]], (['assign'|[H|[Y]]], A)) :- isVar(H), ==(T, '='), mathExpression(TS, (Y, A)), !.
getStatement([[H|T]|TS], (Y, TS)) :- createStatements([[H|T]], [Y|_]).

executeStatements([]) :- !.
executeStatements(L) :- executeStatementsHelper(L, [], _).

executeStatementsHelper([], V, V) :- !.
executeStatementsHelper([[]], V, V) :- !.
executeStatementsHelper([['assign'|[X|E]]|TS], V, Q) :- evaluateMathExpression(E, V, N),  addVar(X, N, V, W), executeStatementsHelper(TS, W, Q), !.
executeStatementsHelper([['print'|T]|TS], V, Q) :- executePrintStatement(T, V), display('\n'), executeStatementsHelper(TS, V, Q), !.
executeStatementsHelper([['if'|T]|TS], V, Q) :- executeIfStatement(T, V, A), dumbestFunctionIveEverWritten(A,B), append(B, TS, C), executeStatementsHelper(C, V, Q).
executeStatementsHelper([['while'|T]|TS], V, Q) :- executeWhileStatement(T, V, P), executeStatementsHelper(TS, P,Q).

executeWhileStatement([M|B], V, Q) :- evaluateMathExpression(M, V, R), \==(R, 0), dumbestFunctionIveEverWritten(B, C), executeStatementsHelper(C, V, P), executeWhileStatement([M|B], P, Q), !.
executeWhileStatement([_|_], V, V).

executeIfStatement([M|[_|F]], V, F) :- evaluateMathExpression(M, V, R), ==(R, 0), !.
executeIfStatement([_|[T|_]], _, T).

executePrintStatement([], _) :- !.
executePrintStatement([H|T], V) :- evaluateMathExpression(H, V, N), display(N), display(' '), executePrintStatement(T, V).

removeVar(_, [], []) :- !.
removeVar(X, [(X, _)|T], T) :- !.
removeVar(X, [(V, N)|T], [(V, N)|R]) :- removeVar(X, T, R).

addVar(X, N, V, [(X, N)|Z]) :- removeVar(X, V, Z).

mathExpression([], ([], [])) :- !.
mathExpression(X, (Y, Z)) :- mathExpressionSplitter(X, (A, Z)), mathExpressionCompiler(A, Y).

mathExpressionSplitter([], ([], [])) :- !.
mathExpressionSplitter([H], ([H], [])) :- !.
mathExpressionSplitter([H|T], ([H|Y], Z)) :- isMathItem(H), mathExpressionSplitter(T, (Y, Z)), !.
mathExpressionSplitter([H|[T|TS]], ([H|Y], Z)) :- isMathVar(H,T), mathExpressionSplitter([T|TS], (Y, Z)), !.
mathExpressionSplitter([[H|T]|TS], ([[H|T]|Y], Z)) :- listIsMathExperession([H|T]), mathExpressionSplitter(TS, (Y, Z)), !.
mathExpressionSplitter([H|T], ([], [H|T])) :- not(isMathItem(H)).

listIsMathExperession([_]) :- !.
listIsMathExperession([H|_]) :- isMathItem(H), !.
listIsMathExperession([X|[Y|_]]) :-  isMathVar(X, Y).

isMathItem(X) :- isOp(X), !.
isMathItem(X) :- isNum(X).

isMathVar(X, Y) :- isVar(X), \==(Y, '=').

pemdas(X, 6) :- not(isOp(X)), !.
pemdas(X, 0) :- ==(X, '||'), !.
pemdas(X, 1) :- ==(X, '&&'), !.
pemdas(X, 2) :- mem(X, ['==', '!=']), !.
pemdas(X, 3) :- mem(X, ['<', '<=', '>', '>=']), !.
pemdas(X, 4) :- mem(X, ['+', '-']), !.
pemdas(X, 5) :- mem(X, ['*', '/', '%']).

minOpPriority([], _) :- display('tried to get min priority of empty list\n'), fail, !.
minOpPriority([X], R) :- pemdas(X, R), !.
minOpPriority([H|T], R) :- pemdas(H, A), minOpPriority(T, B), min(A, B, R).

mathExpressionCompiler([], []) :- !.
mathExpressionCompiler([[H|T]], R) :- mathExpressionCompiler([H|T], R), !.
mathExpressionCompiler([X], X) :- atom(X), !.
mathExpressionCompiler(L, [X|[Y|[Z]]]) :- reverse(L, R), minOpPriority(L, M), firstSatisfying(R, pemdas, M, X), splitBy(R, X, (A, B)), reverse(A, D), reverse(B, C), mathExpressionCompiler(C, Y), mathExpressionCompiler(D, Z).

getVar([(V, N)|_], V, N) :- !.
getVar([(_, _)|VS], Q, R) :- getVar(VS, Q, R).

evaluateMathExpression(X, V, R) :- isVar(X), getVar(V, X, R), !.
evaluateMathExpression(X, _, R) :- isNum(X), number_atom(R, X), !.
evaluateMathExpression(X, _, X) :- number(X), !.
evaluateMathExpression([X], V, R) :- evaluateMathExpression(X,V,R), !.
evaluateMathExpression(['+'|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), R is A+B, !.
evaluateMathExpression(['-'|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), R is A-B, !.
evaluateMathExpression(['*'|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), R is A*B, !.
evaluateMathExpression(['/'|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), R is A div B, !.
evaluateMathExpression(['%'|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), R is A rem B, !.
evaluateMathExpression(['<'|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), lessThan(A,B,R), !.
evaluateMathExpression(['>'|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), moreThan(A,B,R), !.
evaluateMathExpression(['<='|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), lessThanOrEqual(A,B,R), !.
evaluateMathExpression(['>='|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), moreThanOrEqual(A,B,R), !.
evaluateMathExpression(['=='|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), equals(A,B,R), !.
evaluateMathExpression(['!='|[Y|Z]], V, R) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), notEquals(A,B,R), !.
evaluateMathExpression(['&&'|[Y|Z]], V, 1) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), B > 0, A > 0, !.
evaluateMathExpression(['&&'|[_|_]], _, 0) :- !.
evaluateMathExpression(['||'|[Y|Z]], V, 0) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), A == 0, B == 0, !.
evaluateMathExpression(['||'|[_|_]], _, 1) :- !.

lessThan(A,B,1) :- A < B, !.
lessThan(_,_,0).

moreThan(A,B,1) :- A > B, !.
moreThan(_,_,0).

lessThanOrEqual(A,B,1) :- A =< B, !.
lessThanOrEqual(_,_,0).

moreThanOrEqual(A,B,1) :- A >= B, !.
moreThanOrEqual(_,_,0).

and(A,B,1) :- A > 0, B > 0, !.
and(_,_,0).

or(A,_,1) :- A > 0, !.
or(_,B,1) :- B > 0, !.
or(_,_,0).

equals(A,B,1) :- A == B, !.
equals(_,_,0).

notEquals(A,B,1) :- A \== B, !.
notEquals(_,_,0).
