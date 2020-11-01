:- ensure_loaded('inventory.pl').
:- ensure_loaded('taken-item-positions.pl').

desired_items(['G', 'S', 'B']).
undesired_items(['X', 'Y', 'Z']).

check_for_dropped_item(R, C) :-
    square(R, C, Item),
    desired_items(DI),
    undesired_items(UI),
    (
        member(Item, DI) -> item_type(Item, Name),
                            format('You found a desired item: ~w', [Name]),
                            save_item_to_inventory(Item),
                            save_item_position(R, C, Item)
        ; member(Item, UI) -> lose_item(Item)
        ; write('You moved to an empty square')
    ),
    nl,
    print_inventory,
    nl.

print_inventory :-
    findall(inventory_item(I), inventory_item(I), Items),
    inventory_to_item_names(Items, ItemNames),
    format('Your Inventory: ~w', [ItemNames]), nl.

inventory_to_item_names([], []).
inventory_to_item_names([II|T], [I|L]) :-
    item_name_from_inventory_item(II, I),
    inventory_to_item_names(T, L).

item_name_from_inventory_item(inventory_item(I), N) :-
    item_type(I, N).

lose_item(ItemFound) :-
    write('Losing item'), nl,
    item_type(ItemFound, ItemFoundName),
    write(ItemFoundName), nl,
    findall(inventory_item(I), inventory_item(I), InventoryItems),
    write(InventoryItems), nl,
    inventory_to_item_names(InventoryItems, InventoryItemNames),
    write(InventoryItemNames), nl,
    format('You found an undesired item: ~w.', [ItemFoundName]), nl,
    format('Select an item from your inventory to lose: ~w.', [InventoryItemNames]), nl,
    format('Type "g." for Gold, "s." for Silver or "b." for Bronze.'), nl,
    read(ITL),
    upcase_atom(ITL, UITL),
    remove_item_from_inventory(UITL),
    item_name_from_inventory_item(inventory_item(UITL), ItemDroppedName),
    taken_item_position(R, C, UITL),
    replace_square(R, C, UITL),
    remove_item_position(R, C, UITL),
    format('The ~w you chose to drop has been returned to its original position.', [ItemDroppedName]).


save_item_to_inventory(I) :-
    assertz(inventory_item(I)),
    persist_inventory.

remove_item_from_inventory(I) :-
    retract(inventory_item(I)),
    persist_inventory.

save_item_position(R, C, I) :-
    assertz(taken_item_position(R, C, I)),
    persist_taken_item_positions.

remove_item_position(R, C, I) :- % remove the position (taken_item_position) from the DB
    retract(taken_item_position(R, C, I)),
    persist_taken_item_positions.

empty_inventory :-
    findall(inventory_item(I), inventory_item(I), Items),
    remove_inventory_items(Items),
    persist_inventory.

remove_inventory_items([]).
remove_inventory_items([I|T]) :-
    retract(I),
    remove_inventory_items(T).

clear_taken_item_positions :-
    findall(taken_item_position(R, C, I), taken_item_position(R, C, I), Positions),
    remove_taken_item_position(Positions),
    persist_taken_item_positions.

remove_taken_item_position([]).
remove_taken_item_position([P|T]) :-
    retract(P),
    remove_taken_item_position(T).

persist_inventory :-
    tell('inventory.pl'),
    write(':- dynamic(inventory_item/1).'), nl,
    listing(inventory_item),
    told.

persist_taken_item_positions :-
    tell('taken-item-positions.pl'),
    write(':- dynamic(taken_item_position/3).'), nl,
    listing(taken_item_position),
    told.
