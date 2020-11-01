:- ensure_loaded('board.pl').
:- ensure_loaded('squares.pl').
:- ensure_loaded('directions.pl').

start :-
    write('Welcome to the Squares game!'), nl,
    write('What size board would you like to play with? Enter board size:'), nl,
    read(Size),
    (
        not(integer(Size)) -> write('You must enter an integer'), nl
        ; true
    ),
    (
        Size < 3 -> format('The minimum board size is 3x3. You entered the number ~w. Type "start." to try again.', [Size]), nl, fail
        ; Size > 8 -> format('The maximum board size is 8x8. You entered the number ~w. Type "start." to try again.', [Size]), nl, fail
        ; true
    ),
    createBoard(Size),
    place_player_in_center,
    persistBoard,
    nl, nl,
    write('Type "print." to view the board.'), nl,
    write('Type "adj." to see adjacent squares.'), nl,
    write('Type "move." to move in another direction.'), nl,
    write('Type "stop." to quit.'), nl,
    process_option.

process_option :- write('Option? '),
    read(Option),
    option(Option).

place_player_in_center() :-
    move_player_to_square(2, 2).

move_player_to_square(Row, Col) :-
    retract(square(Row, Col, _)),
    assertz(square(Row, Col, 'P')).

empty_square(Row, Col) :-
    retract(square(Row, Col, _)),
    assertz(square(Row, Col, e)).

print_board([]).
print_board([square(_, Col, Item)|T]) :-
    (
        Col = 1 -> nl
        ; true
    ),
    format('~w ', [Item]),
    print_board(T).

get_adjacent_locations([], [], []).
get_adjacent_locations([H|T], L, [H|I]) :-
    is_user_square(H),
    get_adjacent_locations(T, L, I).

get_adjacent_locations([H|T], [H|L], I) :-
    get_adjacent_locations(T, L, I).

is_user_square(square(_, _, Item)) :-
    Item = 'P'.

user_square(Row, Col) :-
    square(Row, Col, 'P').


list_adjacent_options([]).
list_adjacent_options([square(R, C, _)|T]) :-
    user_square(Row, Col),
    (
        R < Row, C = Col ->
            format('You can travel North. (~w, ~w)', [R, C]), nl
        ; R > Row, C = Col ->
            format('You can travel South. (~w, ~w)', [R, C]), nl
        ; R = Row, C < Col ->
            format('You can travel West. (~w, ~w)', [R, C]), nl
        ; R = Row, C > Col ->
            format('You can travel East. (~w, ~w)', [R, C]), nl
        ;
            write('Received unknown argument.'), nl
    ),
%    format('You can travel ~w', []), nl,
    list_adjacent_options(T).


option(print) :-
    findall(square(Row, Col, Item), square(Row, Col, Item), Squares),
    msort(Squares, Sorted),
    print_board(Sorted),
    nl,
    nl,
    process_option.

option(adj) :- % List squares that are adjacent to the player
    user_square(R, C),
    findall(square(Row, C, Item), square(Row, C, Item), Horizontal),
    get_adjacent_locations(Horizontal, HReduced, _),
    findall(square(R, Col, Item), square(R, Col, Item), Vertical),
    get_adjacent_locations(Vertical, VReduced, _),
    list_adjacent_options(HReduced),
    list_adjacent_options(VReduced),
    process_option.

option(move) :-
    write('Which direction? '), nl,
    write('Type "north.", "south.", "east." or "west." to move.'), nl,
    read(Direction),
    move(Direction),
    write('Successfully moved.'),nl,
    process_option.

option(stop) :-
    tell('squares.pl'),
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
