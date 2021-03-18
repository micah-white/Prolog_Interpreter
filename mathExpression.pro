:- include(listFunctions).

mathExpression([], ([], [])) :- !.
mathExpression(X, (Y, Z)) :- mathExpressionSplitter(X, (A, Z)), mathExpressionCompiler(A, Y).

mathExpressionSplitter([], ([], [])) :- !.
mathExpressionSplitter([H], ([H], [])) :- !.
mathExpressionSplitter([H|T], ([H|Y], Z)) :- isMathItem(H), mathExpressionSplitter(T, (Y, Z)), !.
mathExpressionSplitter([H|[T|TS]], ([H|Y], Z)) :- isMathVar(H,T), mathExpressionSplitter([T|TS], (Y, Z)), !.
mathExpressionSplitter([[H|T]|TS], ([[H|T]|Y], Z)) :- listIsMathExperession([H|T]), mathExpressionSplitter(TS, (Y, Z)), !.
mathExpressionSplitter([H|T], ([], [H|T])) :- not(isMathItem(H)).

listIsMathExperession([X]) :- !.
listIsMathExperession([H|T]) :- isMathItem(H), !.
listIsMathExperession([X|[Y|Z]]) :-  isMathVar(X, Y).

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

getVar([(V, N)|VS], V, N) :- !.
getVar([(V, N)|VS], Q, R) :- getVar(VS, Q, R).

evaluateMathExpression(X, V, R) :- isVar(X), getVar(V, X, R), !.
evaluateMathExpression(X, V, R) :- isNum(X), number_atom(R, X), !.
evaluateMathExpression(X, V, X) :- number(X), !.
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
evaluateMathExpression(['&&'|[Y|Z]], V, 0) :- !.
evaluateMathExpression(['||'|[Y|Z]], V, 0) :- evaluateMathExpression(Y, V, A), evaluateMathExpression(Z, V, B), A == 0, B == 0, !.
evaluateMathExpression(['||'|[Y|Z]], V, 1) :- !.

lessThan(A,B,1) :- A < B, !.
lessThan(A,B,0).

moreThan(A,B,1) :- A > B, !.
moreThan(A,B,0).

lessThanOrEqual(A,B,1) :- A =< B, !.
lessThanOrEqual(A,B,0).

moreThanOrEqual(A,B,1) :- A >= B, !.
moreThanOrEqual(A,B,0).

and(A,B,1) :- A > 0, B > 0, !.
and(A,B,0).

or(A,_,1) :- A > 0, !.
or(_,B,1) :- B > 0, !.
or(_,_,0).

equals(A,B,1) :- A == B, !.
equals(_,_,0).

notEquals(A,B,1) :- A \== B, !.
notEquals(_,_,0).
