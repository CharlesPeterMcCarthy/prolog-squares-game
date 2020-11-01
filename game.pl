:- ensure_loaded('board.pl').
:- ensure_loaded('squares.pl').
:- ensure_loaded('directions.pl').

start :-
    write('Welcome to the Squares game!.'), nl,
    write('Type "print." to view the board.'), nl,
    write('Type "adj." to see adjacent squares.'), nl,
    write('Type "move." to move in another direction.'), nl,
    write('Type "stop." to quit.'), nl,
    nl,
%    place_player_in_center,
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

%get_player_location(X, Y) :-
%    square(X, Y, 'P').

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
%    print_board2(Sorted),
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
