:- ensure_loaded('inventory.pl').
:- ensure_loaded('taken-item-positions.pl').

% Desired item type symbols
% G = Gold, S = Silver, B = Bronze
desired_items(['G', 'S', 'B']).

% Undesired item type symbols
% X = Glass, Y = Rock, Z = Wood
undesired_items(['X', 'Y', 'Z']).

% Called when the player moves to a square
% R is the row of the square
% C is the column of the square
check_for_dropped_item(R, C) :-
    square(R, C, I),                % Get the item (I) at that square
    desired_items(DI),              % Get desired items (DI)
    undesired_items(UI),            % Get undesired items (UI)
    (
        member(I, DI) ->        % Item (I) is a desired item - Pick it up
                            item_type(I, IN),                                  % Get the full item name (IN)
                            format('You found a desired item: ~w', [IN]), nl,
                            write('It has been added to your inventory.'), nl, nl,
                            save_item_to_inventory(I),                      % Add item to player's inventory
                            empty_square(R, C),                             % Mkae this square empty (e)
                            save_item_position(R, C, I),                    % Keep track of where this item was found
                            check_for_game_end                              % Check if the game has ended (All desired items are collected)
        ; member(I, UI) ->      % Item (I) is an undesired item. The player must lose an item from their inventory if one exists
                            prompt_item_loss(I)
        ; write('You moved to an empty square')     % No item is found in the current square (empty square)
    ),
    nl,
    print_inventory,
    nl.

% Print all of the items contained within the player's inventory (inventory_item facts)
print_inventory :-
    findall(inventory_item(I), inventory_item(I), Items),   % Find all inventory items
    inventory_to_item_names(Items, ItemNames),              % Convert list of inventory item symbols to item names
    format('Your Inventory: ~w', [ItemNames]), nl.

% Recursively get the names of the inventory items passed in to this predicate in the list
% First list: List of inventory_item passed in
% Second list: List of names returned
inventory_to_item_names([], []).
inventory_to_item_names([II|T], [N|L]) :-       % II is the inventory_item taken from the start of the list
    item_name_from_inventory_item(II, N),       % N is the name of the item
    inventory_to_item_names(T, L).

% Get the name of items by item symbol, eg. G = Gold
% I is the item symbol, eg. G
% N is the item name, eg. Gold
item_name_from_inventory_item(inventory_item(I), N) :-
    item_type(I, N).

% This is called when the player has moved to a square containing an undesired item
% IF is the symbol of the item found, eg. X
prompt_item_loss(UIF) :-
    item_type(UIF, IFN),                                                     % Get the item name (IFN)
    findall(inventory_item(I), inventory_item(I), II),                      % Find all inventory items (II)
    inventory_to_item_names(II, IIN),                                       % Convert item symbols to item names (IIN)
    format('You found an undesired item: ~w.', [IFN]), nl, nl,
    (
        inventory_item(_) ->    % If the player has at least one invetory item collected (if at least one inventory_item fact exists)
                            format('Select an item from your inventory to lose: ~w.', [IIN]), nl, nl,
                            format('Type "g." for Gold, "s." for Silver or "b." for Bronze.'), nl, nl,
                            read(ITL),                      % Take in the item (ITL) the player has chosen to lose (g, s or b)
                            upcase_atom(ITL, UITL),         % Change user input to uppercase (UITL) to match stored values (G, S or B)
                            print_line_breaker,
                            desired_items(DI),              % Get desired item symbols (DI)
                            (                               % Check item type exists
                                member(UITL, DI) -> lose_item(UITL, UIF)     % Item is a valid desired item
                                ; format('The value you entered (~w) is an invalid item.', [ITL]), nl, nl, prompt_item_loss(UIF)     % Value entered by user is not a valid item type - Reprompt user
                            )
        ; format('Looks like you don\'t have anything in your inventory to drop right now.'), nl, nl    % Player does not currently have any items collected - Skip this action as they cannot drop any items
    ).

% User has chosen an item to lose
% ITL is the item the player chose to lose, eg. G, S or B
% UIF is the undesired item the player encountered
lose_item(ITL, UIF) :-
    (       % Check the inventory_item exists (User currently has that item)
        inventory_item(ITL) ->          % The inventory_item exists
                                remove_item_from_inventory(ITL),                            % Remove item from player's inventory
                                item_name_from_inventory_item(inventory_item(ITL), IDN),    % Get the name of the item they chose to drop
                                taken_item_position(R, C, ITL),                             % Get the row (R) and column (C) of where the item was originally found
                                replace_square(R, C, ITL),                                  % Place the item back in its original position
                                remove_item_position(R, C, ITL),                            % Delete the item_taken_position fact
                                format('The ~w you chose to drop has been returned to its original position.', [IDN])
        ;                               % The item the player chose to drop does not exist in their inventory
                item_type(ITL, IN),                                                             % Get the name of the item the player chose to drop
                format('You do not currently have any ~w in your inventory to drop.', [IN]),
                nl, nl,
                prompt_item_loss(UIF)                                                           % Prompt the user to drop an item again
    ).

% Persist item found in player's inventory (create inventory_item fact)
% I is the symbol of the item, eg. G, S or B
save_item_to_inventory(I) :-
    assertz(inventory_item(I)),
    persist_inventory.

% Remove item from player's inventory (remove inventory_item fact)
% I is the symbol of the item, eg. G, S or B
remove_item_from_inventory(I) :-
    retract(inventory_item(I)),
    persist_inventory.

% Save the position of the item the player's has found
% R is the row
% C is the column
% I is the item symbol, eg. G
save_item_position(R, C, I) :-
    assertz(taken_item_position(R, C, I)),
    persist_taken_item_positions.

remove_item_position(R, C, I) :- % remove the position (taken_item_position) from the DB
    retract(taken_item_position(R, C, I)),
    persist_taken_item_positions.

% Remove all inventory_item facts
% This is called at the start of a new game to remove items from previous game
empty_inventory :-
    findall(inventory_item(I), inventory_item(I), Items),   % Find all invetory items
    remove_inventory_items(Items),                          % Remove all items
    persist_inventory.

% Recursively remove inventory_item facts
remove_inventory_items([]).
remove_inventory_items([I|T]) :-
    retract(I),                 % Remove inventory_item
    remove_inventory_items(T).

% Remove all taken_item_position facts
% This is called at the start of a new game to remove item positions from previous game
clear_taken_item_positions :-
    findall(taken_item_position(R, C, I), taken_item_position(R, C, I), Positions),
    remove_taken_item_position(Positions),
    persist_taken_item_positions.

% Recursively remove taken_item_position facts
remove_taken_item_position([]).
remove_taken_item_position([P|T]) :-
    retract(P),
    remove_taken_item_position(T).

% Persist inventory_item facts to inventory.pl file
persist_inventory :-
    tell('inventory.pl'),
    write(':- dynamic(inventory_item/1).'), nl,
    listing(inventory_item),
    told.

% Persist taken_item_position facts to taken_item_positions.pl file
persist_taken_item_positions :-
    tell('taken-item-positions.pl'),
    write(':- dynamic(taken_item_position/3).'), nl,
    listing(taken_item_position),
    told.
