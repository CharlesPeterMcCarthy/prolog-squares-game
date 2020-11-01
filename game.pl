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
    get_item_drop_count(C),
    drop_items(C, true),
    drop_items(C, false),
    persistBoard,
    nl, nl,
    write('Type "print." to view the board.'), nl,
    write('Type "adj." to see adjacent squares.'), nl,
    write('Type "move." to move in another direction.'), nl,
    write('Type "stop." to quit.'), nl,
    process_option.

get_item_drop_count(C) :-
    board_size(Size),
    (
        Size < 5 -> C is ceiling(Size / 2)
        ; C is Size
    ).

process_option :- write('Option? '),
    read(Option),
    option(Option).

place_player_in_center() :-
    board_size(Size),
    Halfway is ceiling(Size / 2),
    move_player_to_square(Halfway, Halfway).

move_player_to_square(Row, Col) :-
    retract(square(Row, Col, _)),
    assertz(square(Row, Col, 'P')).

empty_square(Row, Col) :-
    retract(square(Row, Col, _)),
    assertz(square(Row, Col, e)).

replace_square(Row, Col, Item) :-
    retract(square(Row, Col, _)),
    assertz(square(Row, Col, Item)).

drop_item(RandomRow, RandomCol, Desired) :-
    format('Dropping gold at ~wx~w',[RandomRow, RandomCol]), nl,
    empty_square(RandomRow, RandomCol),
    (
        Desired = true -> desired_items(RandomItems)
        ; undesired_items(RandomItems)
    ),
%    desired_items(RandomItems),
    random(0, 3, RI),
    nth0(RI, RandomItems, RandomItem),
    replace_square(RandomRow, RandomCol, RandomItem).

% N is the count of items to drop
% D is a value that specifies whether to drop desired items (true) or undesired items (false)
drop_items(N, D) :-
    format('Dropping item ~w', [N]), nl,
    board_size(BoardSize),
    RangeSize is BoardSize + 1,
    random(1, RangeSize, RandomRow),
    random(1, RangeSize, RandomCol),
    square(RandomRow, RandomCol, ItemToCheck),
    (
        ItemToCheck = e -> drop_item(RandomRow, RandomCol, D), ItemsLeft is N - 1
        ; ItemsLeft is N % ItemsLeft must be instantiated - keep value to prevent losing drop items
    ),
    (
        ItemsLeft > 0 -> drop_items(ItemsLeft, D)
        ; true
    ).

desired_items(['G', 'S', 'B']).
undesired_items(['X', 'Y', 'Z']).

list_non_empty_adjacent_squares :-
    user_square(R, C),
    findall(square(Row, C, Item), square(Row, C, Item), Horizontal),
    get_adjacent_locations(Horizontal, HReduced, _),
    findall(square(R, Col, Item), square(R, Col, Item), Vertical),
    get_adjacent_locations(Vertical, VReduced, _),
    append(HReduced, VReduced, Adj),
    get_non_empty_squares(Adj, _, NonEmpty),
    write(NonEmpty).

get_non_empty_squares([], [], []).
get_non_empty_squares([H|A], [H|E], N) :-
    is_empty_square(H),
    get_non_empty_squares(A, E, N).

get_non_empty_squares([H|A], E, [H|N]) :-
    get_non_empty_squares(A, E, N).

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

is_empty_square(square(_, _, Item)) :-
    write('Item: '), nl,
    write(Item), nl,
    write(e), nl,
    Item = e.

is_non_empty_square(square(_, _, Item)) :-
    Item = not('e').

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
    append(HReduced, VReduced, Adj),
    list_adjacent_options(Adj),
    process_option.

option(non) :- % List squares that are adjacent to the player
    list_non_empty_adjacent_squares.

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
