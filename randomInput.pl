consult(project3).

% fileToString("rTest",A), combineLetters(A,B), group(B, C), mathExpression(C, R).

% append([['print', 'x'], ['assign', 'x', 7]], [['print', 'x', 'y']], R).

% evaluateMathExpression(['||', 1, 0], [], T).

% executeIfStatement(['a'|[print, '1']], [('a', 1)], R).

% evaluateMathExpression([<, 'val', 100], [('val', 50)], R).

interpret(rTest, 1).

