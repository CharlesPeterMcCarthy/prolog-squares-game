:- ensure_loaded('db.pl').

start :-
    write('Welcome to the Squares game!.'), nl,
    write('Type "print." to view the board.'), nl,
    write('Type "stop." to quit.'), nl,
    nl,
    place_player_in_center,
    process_option.

process_option :- write('Option? '),
    read(Option),
    option(Option).

place_player_in_center() :-
    move_player_to_square(2, 2).

move_player_to_square(Col, Row) :-
    retract(square(Col, Row, _)),
    assertz(square(Col, Row, 'P')),
    process_option.

print_board([]).
print_board([square(_, _, Item)|T]) :-
%    indexOf([square(_, _, Item)|T], square(_, _, Item), A),
%    X is X+1,
%    (Row is 3, nl),
%    write(A), nl,
    write(Item), nl,
    print_board(T).

option(print) :-
    findall(square(Col, Row, Item), square(Col, Row, Item), Squares),
    msort(Squares, Sorted),
    print_board(Sorted),
    process_option.

option(stop) :-
    tell('db.pl'),
    write(':- dynamic(square/3).'), nl,
    listing(square),
    told,
    write('Done.'),nl.



option(list) :- % Not needed - only used for testing
    findall(square(Col, Row, Item), square(Col, Row, Item), Squares),
    write(Squares), nl,
    process_option.

option(sort) :- % Not needed - only used for testing
    findall(square(Col, Row, Item), square(Col, Row, Item), Squares),
    msort(Squares, Sorted),
    write(Sorted), nl,
    process_option.
