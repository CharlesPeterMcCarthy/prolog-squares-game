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

move_player_to_square(Row, Col) :-
write("Here"),   nl,
write(Row),   nl,
write(Col),   nl,
    retract(square(Row, Col, _)),
    assertz(square(Row, Col, 'P')).

empty_square(Row, Col) :-
    retract(square(Row, Col, _)),
    assertz(square(Row, Col, e)).

print_board([]).
print_board([square(_, _, Item)|T]) :-
%    indexOf([square(_, _, Item)|T], square(_, _, Item), A),
%    X is X+1,
%    (Row is 3, nl),
%    write(A), nl,
    write(Item), nl,
    print_board(T).

%print_board2(Squares) :-
%    print_row(Squares, 1),
%    print_row(Squares, 2),
%    print_row(Squares, 3).

%print_row([], _).
%print_row([square(Col, _, Item)|T], R) :-
%    write(R), nl,
%    write(Col), nl,
%    R = Col,
%    write(Item), nl,
%    print_row(T, R).

% Print board: Possibly loop over squares, keep a list of lists. Add square to sub-list by index (row = index)

get_player_location(X, Y) :-
    square(X, Y, 'P').

move(north) :-
    write('Moving north...'), nl,
    square(R, C, 'P'),
    write(R),
    write(C),
    empty_square(2, 2),
    NewRow is R - 1,
    move_player_to_square(NewRow, 2).

option(print) :-
    findall(square(Row, Col, Item), square(Row, Col, Item), Squares),
    msort(Squares, Sorted),
    print_board(Sorted),
%    print_board2(Sorted),
    process_option.

option(adj) :- % List squares that are adjacent to the player
%    Player = get_player_location(X, Y),
%    write(Player), nl,
%    write(X), nl,
%    write(Y), nl,
    findall(square(Row, 2, Item), square(Row, 2, Item), Horizontal),
    findall(square(2, Col, Item), square(2, Col, Item), Vertical),
%    msort(Squares, Sorted),
    write(Horizontal), nl,
    write(Vertical), nl,
    process_option.

option(move) :-
    write('Which direction? '), nl,
    write('Type "north.", "south.", "east." or "west." to move.'), nl,
    read(Direction),
    move(Direction),
    write('Successfully moved.'),nl,
    process_option.

option(stop) :-
    tell('db.pl'),
    write(':- dynamic(square/3).'), nl,
    listing(square),
    told,
    write('Done.'),nl.



option(list) :- % Not needed - only used for testing
    findall(square(Row, Col, Item), square(Row, Col, Item), Squares),
    write(Squares), nl,
    process_option.

option(sort) :- % Not needed - only used for testing
    findall(square(Row, Col, Item), square(Row, Col, Item), Squares),
    msort(Squares, Sorted),
    write(Sorted), nl,
    process_option.
