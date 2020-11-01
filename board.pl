:- ensure_loaded('squares.pl').

createBoard(N) :-
    createBoard(N, N).
createBoard(Row, Col) :-
    createCol(Row, Col),
    NextRow is Row - 1,
    NextRow > 0,
    createBoard(NextRow, Col).

createCol(Row, 1) :-
    assertz(square(Row, 1, e)).
createCol(CurrentRow, CurrentCol) :-
    NextCol is CurrentCol - 1,
    NextCol > 0,
    assertz(square(CurrentRow, CurrentCol, e)),
    createCol(CurrentRow, NextCol).

persistBoard :-
    tell('squares.pl'),
    write(':- dynamic(square/3).'), nl,
    listing(square),
    told,
    write('Board has been created.'), nl.

%clearBoard :-
%    retract(square(_, _, _)),
%    write('Board has been destroyed.'), nl.
