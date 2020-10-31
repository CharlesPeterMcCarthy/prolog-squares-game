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

option(print) :-
    square(_, _, _),
    process_option.

option(stop) :-
    tell('db.pl'),
    write(':- dynamic(square/3).'), nl,
    listing(square),
    told,
    write('Done.'),nl.
