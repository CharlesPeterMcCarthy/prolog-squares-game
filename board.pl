:- ensure_loaded('squares.pl').

createBoard(N) :-
    findall(square(R, Col, Item), square(R, Col, Item), Squares),
    removeSquares(Squares),
    persistBoard,
    createBoard(N, N),
    persistBoard,
    write('Board has been created.'), nl,
    !.

createBoard(1, Col) :-
    createCol(1, Col),
    !.
createBoard(Row, Col) :-
    createCol(Row, Col),
    NextRow is Row - 1,
    createBoard(NextRow, Col).

createCol(Row, 1) :-
    assertz(square(Row, 1, e)),
    !.
createCol(CurrentRow, CurrentCol) :-
    NextCol is CurrentCol - 1,
    assertz(square(CurrentRow, CurrentCol, e)),
    createCol(CurrentRow, NextCol).

persistBoard :-
    tell('squares.pl'),
    write(':- dynamic(square/3).'), nl,
    listing(square),
    told.

clearBoard :-
    findall(square(R, Col, Item), square(R, Col, Item), Squares),
    removeSquares(Squares),
    persistBoard,
    write('Board has been destroyed.'), nl,
    !.

removeSquares([]).
removeSquares([S|T]) :-
    retract(S),
    removeSquares(T).
