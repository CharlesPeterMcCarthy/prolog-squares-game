:- ensure_loaded('board.pl').
:- ensure_loaded('squares.pl').
:- ensure_loaded('directions.pl').
:- ensure_loaded('items.pl').
:- ensure_loaded('item-types.pl').
:- ensure_loaded('inventory.pl').

start :-
    empty_inventory,
    clear_taken_item_positions,
    write('Welcome to the Squares game!'), nl,
    write('What size board would you like to play with? Enter board size:'), nl,
    read(Size),
    (
        not(integer(Size)) -> format('You must enter an integer. You entered: ~w', [Size]), nl, fail
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
    print_board,
    process_option.

% Generare a number of items that should be dropped on the board.
% C is the count of items returned.
% The amount of items to be dropped depends on the size of the board.
% The bigger the board, the more items will be dropped to fill out the board.
get_item_drop_count(C) :-
    board_size(Size),
    (
        Size < 5 -> C is ceiling(Size / 2)
        ; C is Size
    ).

process_option :-
    nl,
    write('Option? '),
    nl, nl,
    write('Type "print." to view the board.'), nl,
    write('Type "adj." to see adjacent squares.'), nl,
    write('Type "nonempty." to see non-empty adjacent squares.'), nl,
    write('Type "move." to move in another direction.'), nl,
    write('Type "stop." to quit.'), nl, nl,
    read(Option),
    print_line_breaker,
    option(Option).

print_line_breaker :-
    write('-------------------------------------------------------'), nl, nl.

place_player_in_center() :-
    board_size(Size),
    Halfway is ceiling(Size / 2),
    move_player_to_square(Halfway, Halfway).

move_player_to_square(Row, Col) :-
    retract(square(Row, Col, _)),
    assertz(square(Row, Col, 'P')),
    persistBoard.

empty_square(Row, Col) :-
    retract(square(Row, Col, _)),
    assertz(square(Row, Col, e)),
    persistBoard.

replace_square(Row, Col, Item) :-
    retract(square(Row, Col, _)),
    assertz(square(Row, Col, Item)),
    persistBoard.

drop_item(RandomRow, RandomCol, Desired) :-
    empty_square(RandomRow, RandomCol),
    (
        Desired = true -> desired_items(RandomItems)
        ; undesired_items(RandomItems)
    ),
    random(0, 3, RI),
    nth0(RI, RandomItems, RandomItem),
    replace_square(RandomRow, RandomCol, RandomItem).

% N is the count of items to drop
% D is a value that specifies whether to drop desired items (true) or undesired items (false)
drop_items(N, D) :-
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

list_non_empty_adjacent_squares :-
    user_square(R, C),
    findall(square(Row, C, Item), square(Row, C, Item), Horizontal),
    get_adjacent_locations(Horizontal, HReduced, _),
    findall(square(R, Col, Item), square(R, Col, Item), Vertical),
    get_adjacent_locations(Vertical, VReduced, _),
    append(HReduced, VReduced, Adj),
    get_non_empty_squares(Adj, _, NonEmpty),
    (
        NonEmpty = [] -> write('Looks like there are no items nearby.'), nl
        ; print_non_empty_adjacent_squares(NonEmpty)
    ).

print_non_empty_adjacent_squares([]).
print_non_empty_adjacent_squares([square(R, C, I)|T]) :-
    user_square(Row, Col),
    item_type(I, ItemName),
    (
        R < Row, C = Col ->
            format('Looks like there might be some ~w up North. (~w, ~w)', [ItemName, R, C]), nl
        ; R > Row, C = Col ->
            format('Looks like there might be some ~w down South. (~w, ~w)', [ItemName, R, C]), nl
        ; R = Row, C < Col ->
            format('Looks like there might be some ~w to the West. (~w, ~w)', [ItemName, R, C]), nl
        ; R = Row, C > Col ->
            format('Looks like there might be some ~w to the East. (~w, ~w)', [ItemName, R, C]), nl
        ;
            true
    ),
    print_non_empty_adjacent_squares(T).

get_non_empty_squares([], [], []).
get_non_empty_squares([H|A], [H|E], N) :-
    is_empty_square(H),
    get_non_empty_squares(A, E, N).

get_non_empty_squares([H|A], E, [H|N]) :-
    get_non_empty_squares(A, E, N).

print_board :-
    findall(square(Row, Col, Item), square(Row, Col, Item), Squares),
    msort(Squares, Sorted),
    print_board_square(Sorted),
    nl, nl.

print_board_square([]).
print_board_square([square(_, Col, Item)|T]) :-
    (
        Col = 1 -> nl, nl
        ; true
    ),
    (
        is_empty_square(square(_, Col, Item)) -> write(' -  ')
        ; format(' ~w  ', [Item])
    ),
    print_board_square(T).

find_adjacent_locations(Adj) :-
    user_square(R, C),
    findall(square(Row, C, Item), square(Row, C, Item), Horizontal),
    get_adjacent_locations(Horizontal, HReduced, _),
    findall(square(R, Col, Item), square(R, Col, Item), Vertical),
    get_adjacent_locations(Vertical, VReduced, _),
    append(HReduced, VReduced, Adj).

get_adjacent_locations([], [], []).
get_adjacent_locations([H|T], L, [H|I]) :-
    is_user_square(H),
    get_adjacent_locations(T, L, I).

get_adjacent_locations([H|T], L, [H|I]) :-
    user_square(UR, UC),
    get_coords(H, R, C),
    (
        R < (UR - 1) ;
        R > (UR + 1) ;
        C < (UC - 1) ;
        C > (UC + 1)
    ),
    get_adjacent_locations(T, L, I).

get_adjacent_locations([H|T], [H|L], I) :-
    get_adjacent_locations(T, L, I).

is_user_square(square(_, _, Item)) :-
    Item = 'P'.

is_empty_square(square(_, _, Item)) :-
    Item = e.

is_non_empty_square(square(_, _, Item)) :-
    Item = not('e').

user_square(Row, Col) :-
    square(Row, Col, 'P').

get_coords(square(R, C, _), Row, Col) :-
    Row is R,
    Col is C.

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
            write('Error - Unknown direction.'), nl
    ),
    list_adjacent_options(T).

check_for_game_end :-
    (
        not(square(_, _, 'G')), not(square(_, _, 'S')), not(square(_, _, 'B')) ->
                format('You win! You collected all desireable items. Congrats!'),
                print_board,
                false
        ; true
    ).

option(print) :-
    print_board,
    process_option.

option(adj) :- % List squares that are adjacent to the player
    find_adjacent_locations(Adj),
    list_adjacent_options(Adj),
    process_option.

option(nonempty) :- % List squares that are adjacent to the player
    list_non_empty_adjacent_squares, nl,
    process_option.

option(move) :-
    write('Which direction would you like to move? '), nl, nl,
    find_adjacent_locations(Adj),
    list_adjacent_options(Adj), nl,
    write('Type "north.", "south.", "east." or "west." to move.'), nl, nl,
    read(Direction),
    print_line_breaker,
    (
        direction(Direction) -> move(Direction)
        ; format('"~w" is not a valid direction! You have not moved anywhere.', [Direction]), nl
    ),
    print_board,
    process_option.

option(inventory) :-
    print_inventory,
    process_option.

option(stop) :-
    write('Thanks for playing! Goodbye.'), nl.
