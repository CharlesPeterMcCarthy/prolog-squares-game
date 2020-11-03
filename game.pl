:- ensure_loaded('board.pl').
:- ensure_loaded('squares.pl').
:- ensure_loaded('directions.pl').
:- ensure_loaded('items.pl').
:- ensure_loaded('item-types.pl').
:- ensure_loaded('inventory.pl').

% This predicate is called to start the game.
% From this point on, the game will prompt the user for any further input.
start :-
    empty_inventory,                % Empty the player's inventory from the previous game
    clear_taken_item_positions,     % Remove the "Taken Item Positions" from the previous game
    write('Welcome to the Squares game!'), nl,
    write('What size board would you like to play with? Enter board size:'), nl,
    read(S),
    (
        not(integer(S)) -> format('You must enter an integer. You entered: ~w', [S]), nl, fail
        ; true
    ),
    (
        S < 3 -> format('The minimum board size is 3x3. You entered the number ~w. Type "start." to try again.', [S]), nl, fail
        ; S > 8 -> format('The maximum board size is 8x8. You entered the number ~w. Type "start." to try again.', [S]), nl, fail
        ; true
    ),
    organiseBoard(S),
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

% Prompt the user to enter an option
process_option :-
    nl,
    write('Option? '),
    nl, nl,
    write('Type "print." to view the board.'), nl,
    write('Type "adj." to see adjacent squares.'), nl,
    write('Type "nonempty." to see non-empty adjacent squares.'), nl,
    write('Type "inventory." to see your inventory items.'), nl,
    write('Type "move." to move in another direction.'), nl,
    write('Type "stop." to quit.'), nl, nl,
    read(Option),
    print_line_breaker,
    option(Option).

print_line_breaker :-
    write('-------------------------------------------------------'), nl, nl.

% Place the player in the center (or as close as possible) of the board.
% An odd integer sized board will land the player in the very center, ie. Size 3, 5, 7.
% An even integer sized board will land the player slightly left, slight above center, ie. Size 4, 6, 8
place_player_in_center() :-
    board_size(S),                  % S is the size of the board in length
    H is ceiling(S / 2),            % H is the halfway mark on the board
    move_player_to_square(H, H).

% Remove the square / item at (R, C) and replace with the Player 'P'
move_player_to_square(R, C) :-
    retract(square(R, C, _)),
    assertz(square(R, C, 'P')),
    persistBoard.

% Remove the square / item at (R, C) and replace it with an empty square 'e'
empty_square(R, C) :-
    retract(square(R, C, _)),
    assertz(square(R, C, e)),
    persistBoard.

% Remove the square / item at (R, C) and replace it with an a square containing an Item (I)
replace_square(R, C, I) :-
    retract(square(R, C, _)),
    assertz(square(R, C, I)),
    persistBoard.

% Desired will be 'true' or 'false'
drop_item(R, C, Desired) :-
    empty_square(R, C),
    (
        Desired = true -> desired_items(RandomItems)    % Assign all of the desired item types to RandomItems
        ; undesired_items(RandomItems)                  % Assign all of the undesired item types to RandomItems
    ),
    random(0, 3, RI),                                   % Generate random number between 0 and 2
    nth0(RI, RandomItems, RandomItem),                  % Get the RandomItem from RandomItem at index RI
    replace_square(R, C, RandomItem).

% This predicate is used to drop random items at random place on the board when the game starts
% N is the count of items to drop
% D is a value that specifies whether to drop desired items (true) or undesired items (false)
drop_items(N, D) :-
    board_size(BoardSize),
    RangeSize is BoardSize + 1,                         % Add 1 to the board size to generate random numbers up to and including the board size
    random(1, RangeSize, RandomRow),
    random(1, RangeSize, RandomCol),
    square(RandomRow, RandomCol, ItemToCheck),
    (
        ItemToCheck = e -> drop_item(RandomRow, RandomCol, D), ItemsLeft is N - 1   % Square is empty, proceed to drop item in this square
        ; ItemsLeft is N                                                            % Square is not empty! Do not drop item. This iteration will repeat as we don't decrement ItemsLeft. ItemsLeft must be instantiated - keep value to prevent losing drop items
    ),
    (
        ItemsLeft > 0 -> drop_items(ItemsLeft, D)       % Continue recursion and dropping items until ItemsLeft is 0
        ; true
    ).

% Print out the board layout in its current state
print_board :-
    findall(square(Row, Col, Item), square(Row, Col, Item), Squares),   % Find all squares
    msort(Squares, Sorted),                                             % Sort all of the squares in order to loop through them in order
    print_board_squares(Sorted),
    nl, nl.

% Print each of the sorted squares passed in
print_board_squares([]).
print_board_squares([square(_, Col, Item)|T]) :-    % Destruct the list
    (
        Col = 1 -> nl, nl                           % If Column is 1, this signifies the row has ended. Move down 2 lines in the console to begin printing the next row
        ; true
    ),
    (
        is_empty_square(square(_, Col, Item)) -> write(' -  ')      % Replace empty squares with a dash symbol
        ; format(' ~w  ', [Item])                                   % Print item
    ),
    print_board_squares(T).

% List all of the adjacent squares to the player that have an item on them
% Empty squares are ignored
list_non_empty_adjacent_squares :-
    find_adjacent_locations(Adj),                                                % Find the squares that are directly adjacent to the player (Adj)
    split_empty_and_item_squares(Adj, _, NE),                                    % Get only the adjacent squares that have items and are not empty (NE)
    (
        NE = [] -> write('Looks like there are no items nearby.'), nl           % If NE list is empty and contains no adjacent squares
        ; print_non_empty_adjacent_squares(NE)
    ).

% Loop through the non-empty adjacent squares and display information about the direction and the item contained in that square
print_non_empty_adjacent_squares([]).
print_non_empty_adjacent_squares([square(R, C, I)|T]) :-        % Destruct the list of squares
    user_square(UR, UC),                                        % Get the player's current square
    item_type(I, IN),                                           % Get the full item name (IN) of the item (I) symbol
    (                                                           % Compared the Row and Column values to figure out the direction the item is in & print details about the item
        R < UR, C = UC ->
            format('Looks like there might be some ~w up North. (~w, ~w)', [IN, R, C]), nl
        ; R > UR, C = UC ->
            format('Looks like there might be some ~w down South. (~w, ~w)', [IN, R, C]), nl
        ; R = UR, C < UC ->
            format('Looks like there might be some ~w to the West. (~w, ~w)', [IN, R, C]), nl
        ; R = UR, C > UC ->
            format('Looks like there might be some ~w to the East. (~w, ~w)', [IN, R, C]), nl
        ;
            true
    ),
    print_non_empty_adjacent_squares(T).

% First list: A list of squares
% Second list: The returned list of empty squares
% Third list: The retured list of non-empty squares (containing items)
split_empty_and_item_squares([], [], []).
split_empty_and_item_squares([H|A], [H|E], N) :- % Find the empty squares
    is_empty_square(H),
    split_empty_and_item_squares(A, E, N).

% Find the non-empty squares
split_empty_and_item_squares([H|A], E, [H|N]) :-
    split_empty_and_item_squares(A, E, N).

% Find the squares that are directly adjacent to the player
find_adjacent_locations(Adj) :-
    user_square(R, C),                                                  % Get the player's current position
    findall(square(Row, C, I), square(Row, C, I), H),                   % Find all of the squares (H) on the horizonal line to the player
    get_adjacent_locations(H, HR, _),                                   % Get only the squares (HR) that are horizontally adjacent to the player
    findall(square(R, Col, I), square(R, Col, I), V),                   % Find all of the squares (V) on the vertical line to the player
    get_adjacent_locations(V, VR, _),                                   % Get only the squares (VR) that are vertically adjacent to the player
    append(HR, VR, Adj).                                                % Combine the horizontal and vertical adjacent squares together in a list (Adj)

% First list: A list of squares
% Second list: The returned list of directly adjacent squares
% Third list: The returned list of non-directly adjacent squares (these are ignored)
get_adjacent_locations([], [], []).
get_adjacent_locations([H|T], L, [H|I]) :-
    is_user_square(H),                          % Ignore (third list) this square if it is the Player's current square (H)
    get_adjacent_locations(T, L, I).

get_adjacent_locations([H|T], L, [H|I]) :-
    user_square(UR, UC),
    get_coords(H, R, C),                        % Get the coordinates (R and C) for the square passed in (H)
    (                                           % If any of the follow 4 criteria are true, ignore (third list) this square (H)
        R < (UR - 1) ;
        R > (UR + 1) ;
        C < (UC - 1) ;
        C > (UC + 1)
    ),
    get_adjacent_locations(T, L, I).

get_adjacent_locations([H|T], [H|L], I) :-      % Place this square in the directly adjacent squares list (second list)
    get_adjacent_locations(T, L, I).

% List the squares passed in with the direction in relation to the Player
list_adjacent_options([]).
list_adjacent_options([square(R, C, _)|T]) :-
    user_square(UR, UC),
    (
        R < UR, C = UC ->
            format('You can travel North. (~w, ~w)', [R, C]), nl
        ; R > UR, C = UC ->
            format('You can travel South. (~w, ~w)', [R, C]), nl
        ; R = UR, C < UC ->
            format('You can travel West. (~w, ~w)', [R, C]), nl
        ; R = UR, C > UC ->
            format('You can travel East. (~w, ~w)', [R, C]), nl
        ;
            write('Error - Unknown direction.'), nl
    ),
    list_adjacent_options(T).

% Check if square passed in is the Player's current square
is_user_square(square(_, _, I)) :-
    I = 'P'.

% Check if square passed in is empty (e)
is_empty_square(square(_, _, I)) :-
    I = e.

% Check if square passed in is not empty (e)
is_non_empty_square(square(_, _, I)) :-
    I = not('e').

% Get the Row (R) and Column (C) for the Player's current square
user_square(R, C) :-
    square(R, C, 'P').

% Get the coordinates (Row and Col) for the square passed in
get_coords(square(R, C, _), Row, Col) :-
    Row is R,
    Col is C.

% Check if there are no more desireable items on the board.
% The game ends when there are no more desireable items left.
% Prompt the user to play again.
check_for_game_end :-
    (
        not(square(_, _, 'G')), not(square(_, _, 'S')), not(square(_, _, 'B')) ->
                write('You win! You collected all desireable items. Congrats!'), nl,
                print_board,
                write('Play Again? Type "yes." or "no."'), nl, nl,
                read(PlayAgain),
                play_again(PlayAgain)
        ; true
    ).

% Trigger the game to "start" again (The start predicate will clear all of the previous game data)
play_again(yes) :-
    start.

% Exit the game
play_again(no) :-
    stop_game,
    !.

% Invalid end of game option
play_again(_) :-
    write('That was an invalid option.'), nl, nl,
    check_for_game_end.

stop_game :-
    write('Thanks for playing! Goodbye.'), nl,
    halt.

% User option to print the game board in its current state
option(print) :-
    print_board,
    process_option.

% User option to list the squares that are adjacent to the player
option(adj) :-
    find_adjacent_locations(Adj),
    list_adjacent_options(Adj),
    process_option.

% User option to list the non-empty adjacent squares to the user
option(nonempty) :-
    list_non_empty_adjacent_squares, nl,
    process_option.

% User option to move. This will prompt the user to enter a direction before the movement takes place.
option(move) :-
    write('Which direction would you like to move? '), nl, nl,
    find_adjacent_locations(Adj),
    list_adjacent_options(Adj), nl,
    write('Type "north.", "south.", "east." or "west." to move.'), nl, nl,
    read(Direction),
    print_line_breaker,
    (
        direction(Direction) -> move(Direction) % Move if the direction the entered exists
        ; format('"~w" is not a valid direction! You have not moved anywhere.', [Direction]), nl
    ),
    print_board,
    process_option.

% User option to view their inventory
option(inventory) :-
    print_inventory,
    process_option.

% User option to end the game
option(stop) :-
    stop_game.

% Any other option typed in that does not match any of the specified predicates above
option(_) :-
    write('You have selected an invalid option. Please try again.'), nl, nl,
    process_option.
