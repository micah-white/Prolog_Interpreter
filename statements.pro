:- include(mathExpression).

createStatements([], []) :- !.
createStatements(['print'|T], [['print'|A]|C]) :- createPrintStatement(T, (A, B)), createStatements(B, C),!.
createStatements(['if'|T], [['if'|S]|B]) :- createIfStatement(T, (S, A)), createStatements(A, B), !.
createStatements(['while'|T], [['while'|S]|B]) :- createWhileStatement(T, (S, A)), createStatements(A, B), !.
createStatements([H|[T|TS]], [['assign'|[H|[Y]]]|B]) :- isVar(H), ==(T, '='), mathExpression(TS, (Y, A)), createStatements(A, B), doubleList(B, Z), !.
createStatements([[H|T]|TS], [Y|TS]) :- createStatements([H|T], Y).

createPrintStatement(L, ([A|C], Z)) :- mathExpression(L, (A, [','|B])), createPrintStatement(B, (C, Z)), !.
createPrintStatement(L, (Y, Z)) :- mathExpression(L, (A, Z)), idekFunction(A, B), makeList(B,Y). 

createIfStatement(L, ([V|[X|[Y]]], Z)) :- mathExpression(L, (V, A)), getStatement(A, (X, ['else'|B])), getStatement(B, (Y, Z)), !.
createIfStatement([H|T], ([X|[Y]], Z)) :- mathExpression(H, (X, A)), getStatement(T, (Y, Z)).

createWhileStatement([H|T], ([X|[Y]], Z)) :- mathExpression(H, (X, A)), getStatement(T, (Y, Z)).

getStatement([], ([], [])) :- !.
getStatement(['print'|T], (['print'|A], B)) :- createPrintStatement(T, (A, B)), !.
getStatement(['if'|T], (['if'|S], A)) :- createIfStatement(T, (S, A)), !.
getStatement(['while'|T], R) :- createWhileStatement(T, R), !.
getStatement([H|[T|TS]], (['assign'|[H|[Y]]], A)) :- isVar(H), ==(T, '='), mathExpression(TS, (Y, A)), !.
getStatement([[H|T]|TS], (Y, TS)) :- createStatements([[H|T]], [Y|Z]).

executeStatements([]) :- !.
executeStatements(L) :- executeStatementsHelper(L, [], M).

executeStatementsHelper([], V, V) :- !.
executeStatementsHelper([[]], V, V) :- !.
executeStatementsHelper([['assign'|[X|E]]|TS], V, Q) :- evaluateMathExpression(E, V, N),  addVar(X, N, V, W), executeStatementsHelper(TS, W, Q), !.
executeStatementsHelper([['print'|T]|TS], V, Q) :- executePrintStatement(T, V), display('\n'), executeStatementsHelper(TS, V, Q), !.
executeStatementsHelper([['if'|T]|TS], V, Q) :- executeIfStatement(T, V, A), dumbestFunctionIveEverWritten(A,B), append(B, TS, C), executeStatementsHelper(C, V, Q).
executeStatementsHelper([['while'|T]|TS], V, Q) :- executeWhileStatement(T, V, P), executeStatementsHelper(TS, P,Q).

executeWhileStatement([M|B], V, Q) :- evaluateMathExpression(M, V, R), \==(R, 0), dumbestFunctionIveEverWritten(B, C), executeStatementsHelper(C, V, P), executeWhileStatement([M|B], P, Q), !.
executeWhileStatement([M|B], V, V).



executeIfStatement([M|[T|F]], V, F) :- evaluateMathExpression(M, V, R), ==(R, 0), !.
executeIfStatement([M|[T|F]], V, T).

executePrintStatement([], _) :- !.
executePrintStatement([H|T], V) :- evaluateMathExpression(H, V, N), display(N), display(' '), executePrintStatement(T, V).

removeVar(_, [], []) :- !.
removeVar(X, [(X, N)|T], T) :- !.
removeVar(X, [(V, N)|T], [(V, N)|R]) :- removeVar(X, T, R).

addVar(X, N, V, [(X, N)|Z]) :- removeVar(X, V, Z).
